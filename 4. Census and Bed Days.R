##########################################################
# Name of file: Provisional and Variance Figures
# Data Release: Delayed Discharges monthly publication
# Original author(s): Peter McClurg (spss version: James Mc Nally)
# Original date: 09/10/19 (spss version: 30/11/2017)
# Latest update author (if not using version control)
# Latest update date (if not using version control)
# Latest update description (if not using version control)
# Type of script: Preparation
# Written/run on: R Studio SERVER
# Version of R that the script was most recently run on: ?
# Description of content: Updates trendfile with DD records from most recent publication month.
# Approximate run time: TBC
##########################################################


### 1.Housekeeping ----
# This section should be the only section of the script which requires manual changes 
# for future updates and includes:
#   loading packages
#   setting filepaths and extract dates
#   functions (defined here or sourced from another file)
#   setting plot parameter
#   specifying codes (e.g. ICD-10 codes)



### 1.Filepaths for latest month

filepath<-("/conf/delayed_discharges/RAP development/2019_07/Outputs/")
filepath2<-("/conf/delayed_discharges/RAP development/2019_07/Data/scotland/")

#bring in environment 
source("00_setup_environment.R")


Monthflag<-("Jul 2019")

### 2.Get Scotland_validated file for latest month ----

datafile<-read_spss(paste0(filepath2,"SCOTLAND_validated.sav"))


#strip out code 100s

datafile<-datafile %>% filter(reas1!="Code 100")

#Create new variables

datafile<-datafile %>% mutate(areaname=" ")
datafile<-datafile %>% mutate(year="2019-20")

#rename variables

datafile <- datafile %>% 
  rename(chi = CHINo,
         hbname=Healthboard,
         la=localauthoritycode,
         disch3days=Dischargewithin3daysCensus,
         age=AgeGrouping,
         month=Monthflag,
         daysdelayed=DelayatCensus,
         reas3=DELAY_DESCRIPTION,
         totaPats=NoofPatients,
         delayreason=REASONFORDELAY,
         secReason=REASONFORDELAYSECONDARY)

#save out file for main bed days file


write_sav(datafile,paste0(filepath,"main bed days file.sav")) # save out file


#Save census data file


datafile2 <- filter(datafile, REASONFORDELAYSECONDARY %!in%c("26X","46X") & CENSUSFLAG=="Y")


write_sav(datafile2,paste0(filepath,"main census file.sav")) # save out file



#Create Reason 2 Groupingas

##Sub grouping reas2 for code 9s are currently being classed as H&SC or Pat/Fam reasons. Ensure they are just code 9s.
##Also bring in TRansport as a separate category.


datafile3<-datafile2 %>% mutate(reas2=
                                  if_else(reas3=="Adults with Incapacity Act", "Adults with Incapacity Act",reas2))

datafile3<-datafile2 %>% mutate(reas2=
                                  if_else(reas3!="Adults with Incapacity Act" & reas1=="Code 9", "Other code 9 reasons(not AWI)",reas2))

datafile3<-datafile2 %>% mutate(reas2=
                                  if_else(reas3="Awaiting availability of transport","Transport",reas2))


write_sav(datafile3,paste0(filepath,"main census file Tabs 1 3 6.sav")) # save out file

#Get reason / age breakdown for Scotland.

datafile4$level<-1

datafile4<-datafile4 %>% mutate(areaname=="Scotland")

Scotlandbymainreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE),meandelay=mean(daysdelayed),
            mediandelay=median(daysdelayed)) %>% 
  ungroup()

write_sav(Scotlandbymainreasongrouping,paste0(filepath,"Scotland by main reason grouping.sav")) # save out file


Scotlandbysubreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas2) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE),meandelay=mean(daysdelayed),
            mediandelay=median(daysdelayed)) %>% 
  ungroup()


write_sav(Scotlandbysubreasongrouping,paste0(filepath,"Scotland by sub reason grouping.sav")) # save out file

#Breakdown for HBs (Level=2)
datafile4$level<-"2"
datafile4<-datafile4 %>% mutate(areaname=hbname)


HBbymainreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

write_sav(HBbymainreasongrouping,paste0(filepath,"HB by main reason grouping.sav")) # save out file


HBbysubreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas2) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

write_sav(HBbysubreasongrouping,paste0(filepath,"HB by sub reason grouping.sav")) # save out file


#Breakdown for LAs (Level=2)
datafile4$level<-"3"
datafile4<-datafile4 %>% mutate(areaname=la)


LAbymainreasongrouping <- datafile4 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

write_sav(LAbymainreasongrouping,paste0(filepath,"LA by main reason grouping.sav")) # save out file


{LAbysubreasongrouping <- datafile4 %>% 
    group_by(year,month,level,areaname,age,reas2) %>% 
    summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
              Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
              Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
              Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
              DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
              DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
              DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
              DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
              gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
    ungroup()}

write_sav(LAbysubreasongrouping,paste0(filepath,"LA by sub reason grouping.sav")) # save out file


#add files together

Scot_HB_LA<-bind_rows(Scotlandbymainreasongrouping,Scotlandbysubreasongrouping,HBbymainreasongrouping,HBbysubreasongrouping,
                      LAbymainreasongrouping,LAbysubreasongrouping)

datafile5<-select(Scot_HB_LA,-meandelay,-mediandelay)

#if reas1 is blank, reas1 = reas2

datafile6<-datafile5%>% mutate(reas1=
                                 if_else(is.na(reas1), reas2,reas1))

#remove reas2
datafile6<-select(datafile6,-reas2)
datafile6$age<-"All"
ScotHBLAallages<- datafile6 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

write_sav(ScotHBLAallages,paste0(filepath,"ScotHBLA all ages.sav")) # save out file


ScotHBLAallreasonexcHSCPatFamtotal<-bind_rows(Scot_HB_LA,ScotHBLAallages)
table(ScotHBLAallreasonexcHSCPatFamtotal$reas1) # check on reas1

write_sav(ScotHBLAallreasonexcHSCPatFamtotal,paste0(filepath,"ScotHBLA all reasons exc HSC PatFam total.sav")) # save out file


#Calculate total number of delays excluding code9s.
datafile7<-filter(datafile6, reas1%in%c("Health and Social Care Reasons","Patient/Carer/Family-related reasons"))
datafile7$reas1<-"All Delays excl. Code 9"

ScotHBLAallreasonsincHSCPatFamtotal<- datafile7 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

write_sav(ScotHBLAallreasonincHSCPatFamtotal,paste0(filepath,"ScotHBLA all reasons inc HSC PatFam total.sav")) # save out file


#calculate the number of delays for all reasons

datafile8<-filter(ScotHBLAallreasonexcHSCPatFamtotal, reas1%in%c("Health and Social Care Reasons","Code 9",
                                                                 "Patient/Carer/Family-related reasons"))
table(datafile8$reas1) # check totals against the syntax output ( 320 total is correct )

datafile8$reas1<-"All"
ScotHBLAallreasonsalldelaystotal<- datafile8 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(totpats=sum(totpats,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#add files
Tabs136<-
  bind_rows(ScotHBLAallreasonsalldelaystotal,ScotHBLAallreasonsincHSCPatFamtotal,ScotHBLAallreasonexcHSCPatFamtotal)

arrange(Tabs136,areaname,age,reas1) # arrange data in order for tables


write_sav(Tab136,paste0(filepath,"Tabs 1 3 6_R.sav"))

#Tab 4 data - Mean and Media Length of delay
## datafile2 is the main census file

datafile2$level<-1
datafile2$areaname<-"Scotland"
datafile2$age<-"All"

#Create mean/median delay for following high level groupings in reas1
#Code 9 / Health and Social Care Reasons/Patient/Carer/Family-related reasons

Scotlandhighlevelgrouping <- datafile2 %>% 
  group_by(year,month,level,areaname,age,reas1) %>% 
  summarise(meandelay=mean(daysdelayed),
            mediandelay=median(daysdelayed)) %>% 
  ungroup()


write_sav(Scotlandhighlevelgrouping,paste0(filepath,"Scotland high level groupings.sav")) # save out file







###bed days

#Get filespath+SCOTLAND_PROVISIONAL_TEMP.SAV

datafile11<-read_spss(paste0(filepath,"SCOTLAND_PROVISIONAL_TEMP_R.sav"))

datafile11<-datafile11 %>% mutate(hbname=substring(hbname, 5)) # change hbname and remove the 'NHS_'

hbbeddaysdiffage<- datafile11 %>% 
  group_by(hbname,age,reas1) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth,na.rm=TRUE)) %>% 
  ungroup()


write_sav(hbbeddaysdiffage,paste0(filepath,"hb bed days diff age_R.sav")) # save out file

labeddaysdiffage<- datafile11 %>% 
  group_by(la,age,reas1) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth,na.rm=TRUE)) %>% 
  ungroup()


write_sav(labeddaysdiffage,paste0(filepath,"la bed days diff age_R.sav")) # save out file

datafile12<-datafile11 %>% mutate(hbname="Scotland")

Scotbeddaysdiffage<- datafile12 %>% 
  group_by(hbname,age,reas1) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth,na.rm=TRUE)) %>% 
  ungroup()


write_sav(Scotbeddaysdiffage,paste0(filepath,"Scot bed days diff age_R.sav")) # save out file

datafile13<-rbind(Scotbeddaysdiffage,hbbeddaysdiffage)

datafile13<-datafile13 %>% mutate(age="All")

Scotlandhbbeddays<- datafile13 %>% 
  group_by(hbname,age,reas1) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth,na.rm=TRUE)) %>% 
  ungroup()

datafile14<-rbind(Scotlandhbbeddays,Scotbeddaysdiffage,hbbeddaysdiffage)

write_sav(datafile14,paste0(filepath,"Scot and hb bed days_R.sav")) # save out file

datafile15<-datafile14 %>% mutate(reas1="All")

ScotlandhbAllagesbeddays<- datafile15 %>% 
  group_by(hbname,age,reas1) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth,na.rm=TRUE)) %>% 
  ungroup()

datafile16<-rbind(ScotlandhbAllagesbeddays,datafile14)

write_sav(datafile16,paste0(filepath,"Scot and hb bed days all ages all reasons.sav")) # save out file

datafile17<-labeddaysdiffage %>% mutate(age="All")

labeddaysallages<- datafile17 %>% 
  group_by(la,age,reas1) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth,na.rm=TRUE)) %>% 
  ungroup()

datafile18<-rbind(labeddaysallages,labeddaysdiffage)

write_sav(datafile18,paste0(filepath,"la bed days.sav")) # save out file

datafile19<-datafile18 %>% mutate(reas1="All")

labeddaysallreas1<- datafile19 %>% 
  group_by(la,age,reas1) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth,na.rm=TRUE)) %>% 
  ungroup()

datafile20<-rbind(labeddaysallreas1,datafile18)

write_sav(datafile20,paste0(filepath,"la bed days all ages all reasons.sav")) # save out file

#match in to HB data sheet format

bhbbedstepmplate<-read_spss(paste0(filepath,"hb bed days template.sav"))

#amend variables as necessary in order for matching to work

datafile21 <- left_join(datafile16,bhbbedstepmplate,
                      by = c(("hbname" = "hbname"), ("age"="age"),("reas1"="reas1")))

arrange(datafile21,hbname,age,reas1) # arrange data in order for tables

write_sav(datafile21,paste0(filepath,"hb bed days data sheet minus standard.sav")) # save out file
datafile21<-datafile21%>% mutate(hbname=toupper(hbname),age=toupper(age),reas1=toupper(reas1))

#Add in total for Standard filter on any row that isn't all
datafile22a<-filter(datafile21,reas1!="ALL") 
datafile22<-datafile22a%>% mutate(reas1=
                                    if_else(reas1%in%c("HEALTH AND SOCIAL CARE REASONS","PATIENT/CARER/FAMILY-RELATED REASONS"),"STANDARD","CODE 9"))
table(datafile22$reas1)

#select standard only.
datafile23<-filter(datafile22,reas1=="STANDARD")
# aggregate 

datafile24<- datafile23 %>% 
  group_by(hbname,age,reas1) %>% 
  summarise(OBDs_intheMonth=sum(OBDs_intheMonth,na.rm=TRUE)) %>% 
  ungroup()

#datafile24<-datafile24%>%rename(OBDs2=OBDs_intheMonth)

datafile25<- bind_rows(datafile24, datafile21)
datafile25<-datafile25%>% mutate(hbname=toupper(hbname),age=toupper(age),reas1=toupper(reas1))


arrange(datafile25,hbname,age,reas1) # issue here is that the rows with zeros don't appear so have to match to output file

datafile26<-read.csv(paste0(filepath2,"hb_template.csv"))
datafile26<-datafile26%>% mutate(hbname=toupper(hbname),age=toupper(age),reas1=toupper(reas1))

#match files


datafile27 <- right_join(datafile25, datafile26,
                        by = c(("hbname" = "hbname"), ("age"="age"),("reas1"="reas1")))

#recode any NA as 0
datafile27$OBDs_intheMonth[is.na(datafile27$OBDs_intheMonth)] <- 0

HB_OBD<-select(datafile27,-obds2)

write.xlsx(HB_OBD,paste0(filepath,"HB bed days all ages all reasons_R.xlsx"))


#LA bed days

labedstemplate<-read.csv(paste0(filepath2,"la_template.csv"))

labedstemplate<-labedstemplate%>% mutate(la=toupper(la),age=toupper(age),reas1=toupper(reas1))
datafile20<-datafile20%>% mutate(la=toupper(la),age=toupper(age),reas1=toupper(reas1))



#match with la bed days file

LA_OBD <- merge(datafile20,labedstemplate,all=TRUE) %>% 
                        mutate(OBDs_intheMonth=ifelse(is.na(OBDs_intheMonth),obds2,
                           OBDs_intheMonth)) %>% 
                             select(-obds2)

write.xlsx(LA_OBD,paste0(filepath,"LA bed days all ages all reasons_R.xlsx"))

##LA file has no standard totals - needs created

### END OF SCRIPT ###