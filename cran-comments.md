## Version Update (0.4.3)

This is our third re-submission. We have made substantial changes, based on the previous reviewer's feedback. They provided three helpful points, which we responde to below:

1. Please add more details about the package functionality in your
Description text.

If there are references describing the (theoretical background of)
methods in your package, please add these in the Description field of
your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

RESPONSE:


2. Your examples are wrapped in \dontrun{}, hence nothing gets tested.
Please unwrap the examples if that is feasible and if they can be
executed in < 5 sec for each Rd file or create additionally small toy
examples. Something like
\examples{
    examples for users and checks:
    executable in < 5 sec
    \dontshow{
        examples for checks:
        executable in < 5 sec together with the examples above
        not shown to users
    }
    donttest{
        further examples for users (not used for checks)
    }
    if(interactive()){
        interactive examples (not used for checks)
    }
}
would be desirable.

RESPONSE:

3. Please ensure that you do not use more than 2 cores in your examples.

Please ensure that your examples are executable.  We get:

doc2concrete(feedback_dat$feedback, domain="open")
Error in parallel::mclapply(texts, word_list, wordlist = wordlist,
stop.words = stop.words,  :
   'mc.cores' > 1 is not supported on Windows

RESPONSE:

## Version Update (0.4.1)

This is our second re-submission. We have fixed the LICENSE file issue.
We have also rephrased the description.

## Version Update (0.4.0)

This is our first re-submission. We have added a vignette, and ran the check_win_devel() test to confirm the previous note was fixed.
