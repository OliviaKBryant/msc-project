This folder is a placeholder to be populated with CPRD Aurum data. In order to run the analysis without any changes, this folder should be populated with data in this structure: 


├── datafiles
│   ├── denoms - these files contained the population sizes for each stratifier and category per week. The strata were age group, sex, ethnicity, and region.
│   ├── ethnicity 
│   │   ├── in – contained .txt files of the patient IDs, event dates, and medical codes identifying the condition for each patient for each ethnicity category.
│   │   └── out - .dta file of patient IDs and their corresponding ethnicity
│   └── mental	 
│       ├── in – contained .txt files summarised the data extraction process for each outcome. Observation .dta files were provided for each outcome which is the raw data of interest from CPRD containing the patient id, the medical code, and the event date. Results .dta files were provided which were just list of patient ids who were in that extraction.  
![image](https://user-images.githubusercontent.com/31545379/190512995-e31aafca-c173-4f74-b5e9-e5f9474985c5.png)

