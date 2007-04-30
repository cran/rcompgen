
## support functions that attempt to provide tools useful specifically
## for the Windows Rgui.


## Note: even though these are unexported functions, changes in the
## API should be noted in man/rcompgen.Rd


.win32consoleCompletion <-
    function(linebuffer, cursorPosition,
             check.repeat = TRUE,
             minlength = -1)
{
    isRepeat <- ## is TAB being pressed repeatedly with this combination?
        if (check.repeat)
            (linebuffer == .CompletionEnv[["linebuffer"]] &&
             cursorPosition == .CompletionEnv[["end"]])
        else TRUE

    .assignLinebuffer(linebuffer)
    .assignEnd(cursorPosition)
    .guessTokenFromLine()
    token <- .CompletionEnv[["token"]]
    comps <-
        if (nchar(token) < minlength) character(0)
        else
        {
            .completeToken()
            .retrieveCompletions()
        }

    ## FIXME: no idea how much of this is MBCS-safe

    if (length(comps) == 0)
    {
        ## no completions
        addition <- ""
        possible <- character(0)
    }
    else if (length(comps) == 1)
    {
        ## FIXME (maybe): in certain cases the completion may be
        ## shorter than the token (e.g. when trying to complete on an
        ## impossible name inside a list).  It's debatable what the
        ## behaviour should be in this case, but readline and Emacs
        ## actually delete part of the token (at least currently).  To
        ## achieve this in Rgui one would need to do somewhat more
        ## work than I'm ready to do right now (especially since it's
        ## not clear that this is the right thing to do to begin
        ## with).  So, in this case, I'll just pretend that no
        ## completion was found.

        addition <- substr(comps, nchar(token) + 1, 100000)
        possible <- character(0)
    }
    else if (length(comps) > 1)
    {
        ## more than one completion.  The right thing to is to extend
        ## the line by the unique part if any, and list the multiple
        ## possibilities otherwise.

        additions <- substr(comps, nchar(token) + 1, 100000)
        if (length(table(substr(additions, 1, 1))) > 1)
        {
            ## no unique substring
            addition <- ""
            possible <-
                if (isRepeat) capture.output(cat(format(comps, justify = "left"), fill = TRUE))
                else character(0)
        }
        else
        {
            ## need to figure out maximal unique substr
            keepUpto <- 1
            while (length(table(substr(additions, 1, keepUpto))) == 1) keepUpto <- keepUpto + 1
            addition <- substr(additions[1], 1, keepUpto - 1)
            possible <- character(0)
        }
    }
    list(addition = addition,
         possible = possible,
         comps = paste(comps, collapse = " "))
}


