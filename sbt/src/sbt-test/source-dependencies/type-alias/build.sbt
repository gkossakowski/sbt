logLevel in compile := Level.Debug

incOptions := incOptions.value.copy(apiDebug = true)

scalacOptions += "-Xprint:typer"

scalacOptions += "-Ydebug"

scalacOptions += "-Ylog:xsbt-api"

scalacOptions += "-verbose"
