
outcome <- read.csv("~/Documents/Machine Learning/Tarea1/outcome-of-care-measures.csv")
View(outcome)
attach(outcome)
install.packages("lsr")

rankhospital=function (X, Y, Z){
  library("lsr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
  heartattack= as.double(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  heartattack[is.na(heartattack)] <- 1000
  heartfailure=as.double(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  heartfailure[is.na(heartfailure)] <- 1000
  Neumonia=as.double(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  Neumonia[is.na(Neumonia)] <- 1000
  if (Y=="heart attack") {
    Y=heartattack} else
      if (Y=="pneumonia") {
        Y=Neumonia} else
          if (Y=="heart failure") {
            Y=heartfailure} else 
              stop ("invalid outcome")
  if (X != "AL" & X!= "AK"& X!= "AR" & X!="AZ"& X!= "CA"& X!= "CO" & X!="CT"& X!= "DC"& X!= "DE" & X!="FL" & X!="GA" & X!="GU" & X!="HI" & X!="IA"
      & X!="ID"& X!= "IL" & X!="IN"& X!= "KS"& X!= "KY" & X!="LA"& X!= "MA"
      & X!= "MD"& X!= "ME"& X!= "MI" & X!="MN" & X!="MO"& X!= "MS" & X!="MT"& X!= "NC" & X!="ND"& X!= "NE" & X!="NH" & X!="NJ"
      & X!="NM" & X!="NV" & X!="NY" & X!="OH"& X!= "OK"& X!= "OR" & X!="PA"& X!= "PR"& X!= "RI"
      & X!= "SC" & X!="SD"& X!= "TN"& X!= "TX" & X!="UT"& X!= "VA"& X!= "VI"& X!= "VT"& X!= "WA"& X!= "WI" & X!="WV"& X!= "WY") {
    stop ("invalid state")} else
      if (Z=="best") {
        Z=1}
  if (Z=="worst") {
    a=Hospital.Name[State==X]
    b=Y[State==X]
    b=replace(b,b>999,0)
    c=data.frame(a,b)
    sortFrame(c,b)
    d=sortFrame(c,b)
    e=data.frame(d)
    f=tail(e$a,1)
    return (f)
    return (d)}
  else
    a=Hospital.Name[State==X]
  b=Y[State==X]
  c=data.frame(a,b)
  sortFrame(c,b)
  d=sortFrame(c,b,a)
  e=data.frame(d)
  f=e$a[Z]
  return (f)}



rankhospital("NC","heart attack","worst")

