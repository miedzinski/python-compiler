import collections
import dataclasses
import enum
import glob
import itertools
import os.path
import re
import secrets
import subprocess
import tempfile
import typing as t


class Color(enum.Enum):
    BLACK = '\33[30m'
    RED = '\33[31m'
    GREEN = '\33[32m'
    YELLOW = '\33[33m'
    BLUE = '\33[34m'
    VIOLET = '\33[35m'
    BEIGE = '\33[36m'
    WHITE = '\33[37m'

    END = '\33[0m'
    BOLD = '\33[1m'

    def colored(self, text: t.Any) -> str:
        return f'{self.value}{text}{self.__class__.END.value}'


@dataclasses.dataclass
class Case:
    file: str
    name: str
    source: str
    errors: dict
    out: str
    skip: bool


@dataclasses.dataclass
class Result:
    case: Case
    success: bool
    compilation_errors: t.Optional[dict] = None
    internal_error: t.Optional[str] = None
    clang_error: t.Optional[str] = None
    out: t.Optional[str] = None


def split_cases(test: str) -> t.List[t.Tuple[str, t.List[str]]]:
    cases = []
    name = None
    lines = []
    for line in test.splitlines():
        if line.startswith('[case ') and line.endswith(']'):
            if name is not None:
                cases.append((name, lines))
            name = line[len('[case ') : -1]
            lines = [line]
        else:
            lines.append(line)

    cases.append((name, lines))

    return cases


def parse_case(file: str, lines: t.List[str]) -> Case:
    lines = iter(lines)
    header = next(lines)
    name = header[len('[case ') : -1]
    source_lines = list(itertools.takewhile(lambda x: x != '[out]', lines))
    source = '\n'.join(source_lines) + '\n'
    errors = {}
    out_lines = []
    skip = False

    for (idx, line) in enumerate(source_lines, start=1):
        if line == '[skip]':
            skip = True
        match = re.search(r'# (?:E: (?P<error>.*))|(?:O: (?P<out>.*))', line)
        if not match:
            continue
        if error := match['error']:
            errors[idx] = error
        if out_line := match['out']:
            out_lines.append(out_line)

    out = '\n'.join(lines).rstrip()

    assert not (out_lines and out), f'Mixed output definition in {file}::{name}'

    if not out:
        out = '\n'.join(out_lines)
    if out:
        out += '\n'

    return Case(file, name, source, errors, out, skip)


def collect() -> t.DefaultDict[str, t.List[Case]]:
    cases = collections.defaultdict(list)
    for path in glob.glob('tests/*.test'):
        with open(path) as file:
            test = file.read()
        for name, lines in split_cases(test):
            case = parse_case(path, lines)
            cases[path].append(case)

    count = sum(len(test) for test in cases.values())
    print(Color.BOLD.colored(f'collected {count} items'))

    return cases


def parse_compilation_errors(stderr: str) -> t.Optional[t.Dict[int, str]]:
    if not stderr.startswith('Error: Failed to compile'):
        return

    lines = itertools.dropwhile(
        lambda x: x != 'Caused by:', stderr.splitlines()
    )
    next(lines, None)

    errors = {}

    for line in lines:
        msg, line = re.match(
            r'\s+(.*) \(line (\d+), column \d+\)', line
        ).groups()
        errors[int(line)] = msg

    return errors


def print_step(success: bool) -> None:
    if success:
        step = Color.GREEN.colored('.')
    else:
        step = Color.RED.colored('F')
    print(step, end='', flush=True)


def run_case(case: Case, test_dir: str) -> Result:
    if case.skip:
        print(Color.YELLOW.colored('S'), end='')
    nonce = secrets.token_hex(4)
    binary_path = os.path.join(test_dir, f'case-{nonce}')
    source_path = f'{binary_path}.py'
    bitcode_path = f'{binary_path}.bc'

    with open(source_path, 'w') as file:
        file.write(case.source)

    python_proc = subprocess.run(
        ['target/debug/python', '--emit=bc', source_path, '-o', bitcode_path],
        capture_output=True,
    )

    if python_proc.returncode != 0:
        stderr = python_proc.stderr.decode()
        compilation_errors = parse_compilation_errors(stderr)
        success = case.errors and compilation_errors == case.errors
        print_step(success)
        if compilation_errors is not None:
            return Result(case, success, compilation_errors=compilation_errors)
        else:
            return Result(case, success, internal_error=stderr)

    libraries = (
        'python_core',
        'gc',
        'gcc_s',
        'util',
        'rt',
        'pthread',
        'm',
        'dl',
        'c',
    )
    clang_proc = subprocess.run(
        [
            'clang',
            bitcode_path,
            '-o',
            binary_path,
            '-Ltarget/debug',
            *(f'-l{lib}' for lib in libraries),
        ],
        capture_output=True,
    )

    if clang_proc.returncode != 0:
        print_step(False)
        return Result(case, False, clang_error=clang_proc.stderr.decode())

    proc = subprocess.run(
        [binary_path], stdout=subprocess.PIPE, stderr=subprocess.STDOUT
    )

    stdout = proc.stdout.decode()
    success = stdout == case.out
    print_step(success)
    return Result(case, success, out=stdout)


def report_failures(
    failures: t.Iterator[t.Tuple[str, t.Iterator[Result]]]
) -> None:
    for file, results in failures:
        for result in results:
            print(
                Color.RED.colored(
                    f'{result.case.file}::{result.case.name} failed'
                )
            )

            if result.internal_error is not None:
                print(
                    'Encountered internal error:',
                    result.internal_error,
                    sep='\n',
                )

            if result.compilation_errors is not None:
                print(
                    'Expected compilation errors:',
                    result.case.errors,
                    'Actual compilation errors:',
                    result.compilation_errors,
                    sep='\n',
                )

            if result.clang_error is not None:
                print('Clang error:', result.clang_error, sep='\n')

            if result.out is not None:
                print(
                    'Expected output:',
                    result.case.out,
                    'Actual output:',
                    result.out,
                    sep='\n',
                )


def main() -> None:
    tests = collect()
    print()

    results = []
    with tempfile.TemporaryDirectory(prefix='pythontest-') as test_dir:
        for path, cases in tests.items():
            print(path, end=' ')
            for case in cases:
                results.append(run_case(case, test_dir))
            print()

    print()

    failures = itertools.groupby(
        filter(lambda x: not x.success, results), key=lambda x: x.case.file
    )

    report_failures(failures)


if __name__ == '__main__':
    main()
