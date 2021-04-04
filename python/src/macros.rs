#[macro_export]
macro_rules! format_with_node {
    ($node:expr, $fmt:expr, $($arg:tt)*) => {
        format!(
            concat!($fmt, " (line {}, column {})"),
            $($arg)*,
            $node.span.start.line,
            $node.span.start.column
        )
    };
}

#[macro_export]
macro_rules! bail_with_node {
    ($node:expr, $msg:literal $(,)?) => {
        anyhow::bail!(
            concat!($msg, " (line {}, column {})"),
            $node.span.start.line,
            $node.span.start.column,
        )
    };
    ($node:expr, $fmt:expr, $($arg:tt)*) => {
        anyhow::bail!(
            concat!($fmt, " (line {}, column {})"),
            $($arg)*,
            $node.span.start.line,
            $node.span.start.column,
        )
    };
}
