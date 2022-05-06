The models are not currently shared due to confidentiality
concerns. 

These models need to be in a very specific format and
structure. To add a new model, create a new folder in the models
directory and place the original input files in it. Modify the
starter file to write 502 sets (500 bootstraps total) and
optionally add a RNG seed to the dat file (SS >= 3.30.15), then
rerun it. This can be slow but produces an data.ss_new file with
all the simulated data which is plucked out by r4ss and saved as
a new data.ss file.

Copy minimal files, except the data file, into folder called
"blank" which represents the minimal files needed to run the
model (they are not blank -- poorly named).  Rename files as
ss.exe, starter.ss, control.ss, and forecast.ss. This is because
R functions copy these later and need consistent names among
models. In the blank starter file turn off settings to make it
run as fast as possible (no printing, minimal reporting, no new
files etc.). Also change the data input file to "data.ss" as this
is written by an R function later. In the control file set the
variation inflation factors to be 1 for all factors. This is b/c
the bootstrap data sample sizes are already downweighted by this
factor in the new .dat file. So if left as is then SS would
downweight it again, so the sample size would be too small.

Now it should work to run the function `run_model`. Try it once
in serial then in parallel, for both bootstrap types.

The code works for a single iteration by extracting a simulated
data set (model bootstrap) or simulating one (data bootstrap),
and replacing that data and writing that as data.ss along wit
hthe "blank" files above. Then it's run using r4ss tools to do
peels and analysis.


