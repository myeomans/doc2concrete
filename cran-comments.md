## Version Update (0.4.6)

This is our fourth re-submission. We have made substantial changes, based on the previous reviewer's feedback, and we respond to each in line below:

No 1: Please add a space between year and doi.
--> (2014) <doi:10.3758/s13428-013-0403-5>

RESPONSE No 1: Done! Thanks for pointing this out.

No 2: Please always write TRUE and FALSE instead of T and F.
Also never name your variables T or F.

RESPONSE No 2: Good point. I believe I have replaced every instance of T or F with TRUE/FALSE, throughout the package.

No 3: Please document your datasets in more detail.

RESPONSE No 3: I appreciate this suggestion. I've added more description by including text from the paper in the documentation here.

No 4:  Examples with CPU (user + system) or elapsed time > 10s

Based on this feedback, I went back through the code and significantly optimized the slowest parts. It now runs much faster, and the examples should all process quickly. 

## Version Update (0.4.5)

This is our third re-submission. We have made substantial changes, based on the previous reviewer's feedback. We group their responses into three substantial and helpful suggestions, and we respond to each in line below:

No. 1: Please add more details about the package functionality in your
Description text.

If there are references describing the (theoretical background of)
methods in your package, please add these in the Description field of
your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

RESPONSE No. 1: Thank you for this suggestion. We have added more descriptions relevant to the functionality, as well as references to the two most relevant papers.

No. 2: Your examples are wrapped in \dontrun{}, hence nothing gets tested.
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

RESPONSE No. 2: We have overhauled our testing regime. Following your suggestion, our examples can run now (in part because of our adjustment to #3 below). Additionally, we have added some unit testing for the main functions. This spurred us to also change some of the defaults in the function and add more error handling (e.g. for empty strings) to the code base.

No. 3: Please ensure that you do not use more than 2 cores in your examples.

Please ensure that your examples are executable.  We get:

doc2concrete(feedback_dat$feedback, domain="open")
Error in parallel::mclapply(texts, word_list, wordlist = wordlist,
stop.words = stop.words,  :
   'mc.cores' > 1 is not supported on Windows

RESPONSE No. 3: Thank you for this suggestion. We have made the number of cores adjustable, with a default of 1, so that all examples work smoothly.

## Version Update (0.4.1)

This is our second re-submission. We have fixed the LICENSE file issue.
We have also rephrased the description.

## Version Update (0.4.0)

This is our first re-submission. We have added a vignette, and ran the check_win_devel() test to confirm the previous note was fixed.
