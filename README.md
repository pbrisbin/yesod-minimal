> [!NOTE]
> All of my GitHub repositories have been **archived** and will be migrated to
> Codeberg as I next work on them. This repository either now lives, or will
> live, at:
>
> https://codeberg.org/pbrisbin/yesod-minimal
>
> If you need to report an Issue or raise a PR, and this migration hasn't
> happened yet, send an email to me@pbrisbin.com.

# Yesod minimal

The best tool for fixing bugs is the smallest possible reproducing 
example case.

This repo will hold a few separate, tiny and single-file yesod 
applications that can be used to generate tiny example cases in specific 
areas of yesod (hamlet, forms, persistent, etc).

Rather than creating a full-blown scaffold in which to try and 
reproduce, one of the files here can be taken, adjusted, and gisted as 
an example case for the bug you want fixed.

`runhaskell minimal.hs`

I will do my best to keep these compiling on the latest development 
snapshot of yesod -- but I certainly appreciate any help here in the 
form of pull requests.
