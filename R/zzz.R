
.noGenerics <- TRUE

.CompletionEnv <- new.env(parent = emptyenv())

## needed to save some overhead in .win32consoleCompletion
assign("linebuffer", "", env = .CompletionEnv)
assign("end", 1, env = .CompletionEnv)

assign("settings",
       list(ops = TRUE, ns = TRUE,
            args = TRUE, func = FALSE,
            ipck = FALSE, S3 = TRUE, data = TRUE,
            help = TRUE, argdb = TRUE,
            files = .Platform$OS.type == "windows"),
       env = .CompletionEnv)


assign("options",
       list(package.suffix = "::",
            funarg.suffix = "=",
            function.suffix = "("),
       env = .CompletionEnv)

.FunArgEnv <- new.env(hash = TRUE, parent = emptyenv())

.initialize.argdb() ## see argdb.R

