# Initial CPRD Retinopathy exploration

Because I made codelists first, this was my first actual experience with accessing and using CPRD data before attempting survival analysis. It was mostly just me working 
out how to connect to the data, make tables and cache them in MySQL, collect the data into R Studio and just general stuff around that. In terms of what my script (Katie's script is just an example quickly written in a meeting to help me get started) actually looked at/did:
* Found out how many people in the whole cohort had any retinopathy, non-severe retinopathy, and severe retinopathy
* Found out how many people in the whole *diabetes* cohort had any retinopathy, non-severe retinopathy, and severe retinopathy
* Created tables with the earliest severe and non-severe retinopathy codes (ready to use in survival analysis)
* Found out how many people had only one severe or non-severe retinopathy code
