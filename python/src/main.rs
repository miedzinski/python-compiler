use std::env;
use std::path::Path;

use anyhow::{bail, Result};
use clap::{App, Arg};
use inkwell::context::Context;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::OptimizationLevel;

use crate::codegen::Codegen;

mod codegen;
mod error;
mod expression;
mod module;
mod object;

enum FileType {
    LlvmIr,
    LlvmBc,
}

impl FileType {
    fn extension(&self) -> &str {
        match self {
            FileType::LlvmIr => "ll",
            FileType::LlvmBc => "bc",
        }
    }
}

fn main() -> Result<()> {
    let matches = App::new("python")
        .arg(Arg::with_name("INPUT").required(true))
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("FILE")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("emit")
                .long("emit")
                .takes_value(true)
                .possible_values(&["ir", "bc"])
                .default_value("ir"),
        )
        .get_matches();

    let source = Path::new(matches.value_of("INPUT").unwrap());
    if !source.is_file() {
        bail!("Expected file: {:?}", source);
    }
    let filetype = match matches.value_of("emit") {
        Some("ir") => FileType::LlvmIr,
        Some("bc") => FileType::LlvmBc,
        _ => unreachable!(),
    };
    let out = matches
        .value_of("output")
        .map(|s| s.into())
        .unwrap_or_else(|| {
            env::current_dir().unwrap().join(
                Path::new(source)
                    .file_name()
                    .map(|x| Path::new(x).with_extension(filetype.extension()))
                    .unwrap(),
            )
        });

    Target::initialize_all(&InitializationConfig::default());
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &triple,
            TargetMachine::get_host_cpu_name()
                .to_string()
                .split('-')
                .next()
                .unwrap(),
            &TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();
    let target_data = target_machine.get_target_data();

    let ctx = Context::create();
    let builder = ctx.create_builder();
    let module = ctx.create_module("");
    let mut codegen = Codegen::new(&ctx, &builder, &module, &target_data);
    codegen.compile_main(source)?;

    match filetype {
        FileType::LlvmIr => codegen.emit_ir(out),
        FileType::LlvmBc => codegen.emit_bc(out),
    }
}
