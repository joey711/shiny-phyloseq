# Provenance Tracking Overview

You can download a compressed file containing
the complete code and data necessary
to completely reproduce the steps 
that led to your graphical results
during your session.
After successful download/transmission,
this stored code and data is deleted from the server-side file system.

### Archive

- **Compression Type** - The compression format that will be used to store
your provenance archive's code and data.
- **D.L.** - The download button. Press this and you will download the provenance archive.

### Code Preview

- **# Events** - This determines the amount of code (in number of Shiny *events*) 
that are shown for convenience in the main panel area on the right. 
This does not affect the contents of the provenance archive that you will download.
- **Preview Code** - This tells Shiny-phyloseq to show/update the code
shown on the right-hand side.

### Details

The R code shown on the right-hand side of this panel
is simply the last few events logged by Shiny-phyloseq
and shown for convenient inspection only. 
The complete code and data are contained
within the zipped archive file that is downloaded
when you click the button.

If you click the button additional times,
the code and data is re-processed,
and a new archive is produced that is *caught up*
to your last activities.
This means that you can "archive as you go".
