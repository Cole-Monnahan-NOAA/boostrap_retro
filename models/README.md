The models are not currently shared due to confidentiality
concerns. 

These models need to be in a very specific format and
structure. To add a new model, create a new folder in the models
directory and place the original input files in it. Modify the
starter file to write 502 sets (500 bootstraps total) and rerun
it. This can be slow but produces an data.ss_new file with all
the simulated data which is plucked out by r4ss and saved as a
new data.ss file.

Copy minimal files, except the data file, into folder called
"blank." In the starter file turn off settings to make it run as
fast as possible (no printing, minimal reporting, no new files
etc.). Also change the data input file to "data.ss" as this is
written by an R function later.

Now it shoudl work to run the function `run_model`. Try it once
in serial then in parallel, for both bootstrap types.


