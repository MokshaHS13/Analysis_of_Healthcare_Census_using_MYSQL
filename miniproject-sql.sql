
use project;
-- Problem Statement 1:  Jimmy, from the healthcare department, has requested a report that shows
-- how the number of treatments each age category of patients has gone through in the year 2022. 
-- The age category is as follows, Children (00-14 years), Youth (15-24 years), Adults (25-64 years), 
-- and Seniors (65 years and over).Assist Jimmy in generating the report. 
select * from treatment;
select * from patient;
with cte as (select t.treatmentID,t.date as treatment_date,t.patientID,t.diseaseID,t.claimID,p.ssn,p.dob,
year(curdate())-year(dob) as patient_age 
from treatment t join patient p on t.patientID=p.patientID order by patient_age )
SELECT CASE 
    WHEN patient_age BETWEEN 0 AND 14 THEN 'Children' 
    WHEN patient_age BETWEEN 15 AND 24 THEN 'Youth' 
    WHEN patient_age BETWEEN 25 AND 64 THEN 'Adults' 
    ELSE 'Seniors' 
  END AS age_category, COUNT(*) AS number_of_treatments 
FROM cte WHERE YEAR(treatment_date) = 2022 GROUP BY age_category;


-- Problem Statement 2:  Jimmy, from the healthcare department, wants to know which disease is 
-- infecting people of which gender more often.
-- Assist Jimmy with this purpose by generating a report that shows for each disease the 
-- male-to-female ratio. Sort the data in a way that is helpful for Jimmy.

SELECT t.diseaseID, d.diseaseName,
       SUM(CASE WHEN pe.gender = 'male' THEN 1 ELSE 0 END) AS male_count,
       SUM(CASE WHEN pe.gender = 'female' THEN 1 ELSE 0 END) AS female_count,
       (SUM(CASE WHEN pe.gender = 'male' THEN 1 ELSE 0 END) / 
        SUM(CASE WHEN pe.gender = 'female' THEN 1 ELSE 0 END)) AS male_to_female_ratio
FROM treatment t join patient p on t.patientID=p.patientID join person pe on pe.personID=p.patientID join disease d on t.diseaseID=d.diseaseID
GROUP BY t.diseaseID ORDER BY male_to_female_ratio DESC;

-- Problem Statement 3: Jacob, from insurance management, has noticed that insurance claims are not 
-- made for all the treatments. He also wants to figure out if the gender of the patient has any impact
-- on the insurance claim. Assist Jacob in this situation by generating a report that finds for each
-- gender the number of treatments, number of claims, and treatment-to-claim ratio. And notice if there 
-- is a significant difference between the treatment-to-claim ratio of male and female patients.

select gender,count(treatmentID) as total_treatment,count(t.claimid) as total_claim ,
count(treatmentID)/count(t.claimid) as ratio
from treatment t left join claim c on t.claimID=c.claimID  join patient p on t.patientID=p.patientID 
join person pe on p.patientID=pe.personID 
group by gender;

-- Problem Statement 4: The Healthcare department wants a report about the inventory of pharmacies.
-- Generate a report on their behalf that shows how many units of medicine each pharmacy has in their
-- inventory, the total maximum retail price of those medicines, and the total price of all the medicines
-- after discount. 
-- Note: discount field in keep signifies the percentage of discount on the maximum price.

with cte as (
	select p.pharmacyID, p.pharmacyName, k.medicineID ,k.quantity, k.discount, m.maxPrice,
	(maxPrice*(100-discount)/100) as discount_price, 
	quantity*maxPrice as total_max_price
	from pharmacy p join keep k on p.pharmacyID=k.pharmacyID join medicine m on k.medicineID=m.medicineID
	order by pharmacyID,medicineID)
select * , quantity*discount_price as total_discounted_price from cte order by pharmacyID,medicineID;

-- Problem Statement 5:  The healthcare department suspects that some pharmacies prescribe more medicines
-- than others in a single prescription, for them, generate a report that finds for each pharmacy the
-- maximum, minimum and average number of medicines prescribed in their prescriptions.

select pharmacyName, MAX(quantity_count) as max_val, MIN(quantity_count) as min_val, AVG(quantity_count) as average_val
FROM(  SELECT pr.prescriptionID, ph.pharmacyName, COUNT(DISTINCT(c.medicineID)) as quantity_count
	   FROM PHARMACY ph JOIN PRESCRIPTION pr ON ph.pharmacyId = pr.pharmacyId
	   JOIN CONTAIN c ON c.prescriptionId = pr.prescriptionId GROUP BY pr.prescriptionId
) A GROUP BY pharmacyName;

#-------------------------------------------file2-----------------------------------------------------------------------------#
-- Problem Statement 1: A company needs to set up 3 new pharmacies, they have come up with an idea
-- that the pharmacy can be set up in cities where the pharmacy-to-prescription ratio is the lowest
-- and the number of prescriptions should exceed 100. Assist the company to identify those cities
-- where the pharmacy can be set up.

with cte as (select city,count( distinct p.pharmacyId)  as pharmacy_total ,count(prescriptionID)as prescription_total
	FROM pharmacy p join prescription pr on p.pharmacyID=pr.pharmacyID 	join address a on p.addressID=a.addressID
    group by city )
    select *,pharmacy_total/prescription_total as ratio from cte where prescription_total>100 order by ratio limit 3;

-- Problem Statement 2: The State of Alabama (AL) is trying to manage its healthcare resources more
-- efficiently. For each city in their state, they need to identify the disease for which the
-- maximum number of patients have gone for treatment. Assist the state for this purpose.
-- Note: The state of Alabama is represented as AL in Address Table.

with cte as (
	select state,city,t.diseaseID,diseaseName,count(distinct  p.patientID ) as total_patient,count( distinct t.treatmentID) as total_treatment 
	from address a left join person pe on a.addressID=pe.addressID
	join patient p on p.patientID=pe.personID 
	join treatment t on p.patientID=t.patientID
	join disease d on t.diseaseID=d.diseaseID
    group by state,city,diseaseID having state="AL" )   ,
cte2 as (select state,city ,diseaseID,diseaseName,total_patient,total_treatment ,rank() over (partition by city order by total_treatment desc , total_patient desc)as ranking from cte)
select city,count(diseaseId) as total_disease,sum(total_treatment) from cte2  where ranking=1 group by city 
order by sum(total_treatment) desc;

-- Problem Statement 3: The healthcare department needs a report about insurance plans.
-- The report is required to include the insurance plan, which was claimed the most and
-- least for each disease.  Assist to create such a report.

with cte as (
	select i.planName, d.diseaseID, d.diseaseName, count(c.claimID)as  total_claim ,dense_rank() over(partition by d.diseaseID order by count(c.claimID) desc) as ranking1,dense_rank() over(partition by d.diseaseID order by count(c.claimID) ) as ranking2
	from  insuranceplan i join claim c on i.uin=c.uin join treatment t on t.claimID=c.claimID join disease d on d.diseaseID=t.diseaseID
	group by i.planName,d.diseaseID )
select planName,diseaseId, diseaseName,total_claim  from cte where ranking1=1 or ranking2=1 order by total_claim desc; 

-- Problem Statement 4: The Healthcare department wants to know which disease is most likely to infect
-- multiple people in the same household. For each disease find the number of households that has more
-- than one patient with the same disease. 
-- Note: 2 people are considered to be in the same household if they have the same address. 

with cte as (
	select d.diseasename, t.diseaseid, ad.addressid, count(pe.personid) 
	from address ad  join person pe on pe.addressid = ad.addressid 
	join treatment t on t.patientid = pe.personid
	join disease d on t.diseaseid = d.diseaseid 
	group by t.diseaseid, ad.addressid having(count(pe.personid))>1) 
select diseaseid, diseasename, count(addressid) total_household_with_same_disease from cte 
group by diseaseid order by diseaseId ;

-- Problem Statement 5:  An Insurance company wants a state wise report of the treatments to
-- claim ratio between 1st April 2021 and 31st March 2022 (days both included). Assist them to
-- create such a report.

select ad.state, count(tr.treatmentid)/count(tr.claimid) as ratio 
from address ad join person pe on ad.addressid = pe.addressid
join treatment tr on tr.patientid = pe.personid where tr.date >= "2021-04-01" and date <="2022-03-31" group by ad.state;

#-----------------------------------------------File 3------------------------------------------------------------------------------#
-- Problem Statement 1:  Some complaints have been lodged by patients that they have been prescribed
-- hospital-exclusive medicine that they can’t find elsewhere and facing problems due to that. Joshua,
-- from the pharmacy management, wants to get a report of which pharmacies have prescribed
-- hospital-exclusive medicines the most in the years 2021 and 2022. Assist Joshua to generate the
-- report so that the pharmacies who prescribe hospital-exclusive medicine more often are advised to
-- avoid such practice if possible.   

select pr.pharmacyid, count(c.medicineid) as total_medicine from prescription pr join contain c on  pr.prescriptionid = c.prescriptionid
 join medicine m on c.medicineid = m.medicineid  join treatment t on t.treatmentid = pr.treatmentid
where m.hospitalexclusive= "s" and year(t.date) in(2021, 2022) group by pr.pharmacyid order by total_medicine desc;

-- Problem Statement 2: Insurance companies want to assess the performance of their insurance plans.
-- Generate a report that shows each insurance plan, the company that issues the plan, and the number
-- of treatments the plan was claimed for.

select companyName, planName, count(tr.claimID) as Total_claim from insuranceplan ip 
join insurancecompany ic on ip.companyid = ic.companyid
join claim cl on ip.uin = cl.uin
join treatment tr on cl.claimid = tr.claimid group by companyname, planname order by Total_claim desc;

-- Problem Statement 3: Insurance companies want to assess the performance of their insurance plans.
-- Generate a report that shows each insurance company's name with their most and least claimed insurance
-- plans.

with cte as (
	select ic.companyname, ip.planname, count(tr.treatmentid) total_treatment, 
	dense_rank() over(partition by ic.companyname order by count(tr.treatmentid)) "denserank"
	from insuranceplan ip join insurancecompany ic on ip.companyid = ic.companyid
	join claim cl on ip.uin = cl.uin
	join treatment tr on cl.claimid = tr.claimid 
    group by companyname, planname )
select companyname, (select planname from cte a where total_treatment = (select max(total_treatment) from cte b
where companyname = a.companyname) and companyname = b.companyname limit 1) "max claimed",
(select planname from cte a where total_treatment = (select min(total_treatment) from cte 
where companyname = a.companyname) and companyname = b.companyname limit 1) "least claimed" 
from cte b group by companyname;

-- Problem Statement 4:  The healthcare department wants a state-wise health report to assess which
-- state requires more attention in the healthcare sector. Generate a report for them that shows the
-- state name, number of registered people in the state, number of registered patients in the state,
-- and the people-to-patient ratio. sort the data by people-to-patient ratio. 

select ad.state,ad.state, count(pe.personid) as total_person , count(pa.patientid) as total_patient, count(pe.personID)/count(pa.patientID) as ratio_person_to_patient
from address ad join person pe on ad.addressid = pe.addressid
left join patient pa on pe.personid=pa.patientid  group by ad.state order by ratio_person_to_patient desc;

-- Problem Statement 5:  Jhonny, from the finance department of Arizona(AZ), has requested a report that
-- lists the total quantity of medicine each pharmacy in his state has prescribed that falls under Tax
-- criteria I for treatments that took place in 2021. Assist Jhonny in generating the report. 

select ph.pharmacyName, sum(co.quantity) as total_medicine
from address ad join pharmacy ph on ad.addressid = ph.addressid
join prescription pr on pr.pharmacyid = ph.pharmacyid
join contain co on pr.prescriptionid = co.prescriptionid
join treatment tr on tr.treatmentid = pr.treatmentid
join medicine me on me.medicineid = co.medicineid
where year(tr.date) = "2021" and me.taxcriteria = "I" and ad.state = "AZ" group by ph.pharmacyname order by pharmacyName;

#----------------------------------------------SQL CASE--------------------------------------------------------------------------------#
-- Problem Statement 1: “HealthDirect” pharmacy finds it difficult to deal with the product type of medicine being displayed in numerical form, 
-- they want the product type in words. Also, they want to filter the medicines based on tax criteria. 
-- Display only the medicines of product categories 1, 2, and 3 for medicines that come under tax category I and medicines of 
-- product categories 4, 5, and 6 for medicines that come under tax category II.
-- Write a SQL query to solve this problem.

select m.medicineID,productName, (case 
    when producttype = 1 then "generic"
	when producttype = 2 then "patent"
    when producttype = 3 then "reference"
    when producttype = 4 then "similar"
	when producttype = 5 then "new"
    when producttype = 6 then "specific"
    when producttype = 7 then "biological"
    when producttype = 8 then "dinamized"
    end ) product_category, 
(case 
    when producttype in (1,2,3) then 'I'
    when producttype in (4, 5, 6) then 'II' 
    end ) tax_category
from medicine m join keep ke on m.medicineid = ke.medicineid
join pharmacy ph on ke.pharmacyid = ph.pharmacyid where ph.pharmacyname = "healthdirect";

-- Problem Statement 2:  'Ally Scripts' pharmacy company wants to find out the quantity of medicine 
-- prescribed in each of its prescriptions.
-- Write a query that finds the sum of the quantity of all the medicines in a prescription and if the 
-- total quantity of medicine is less than 20 tag it as “low quantity”. If the quantity of medicine is 
-- from 20 to 49 (both numbers including) tag it as “medium quantity“ and if the quantity is more than 
-- equal to 50 then tag it as “high quantity”.
-- Show the prescription Id, the Total Quantity of all the medicines in that prescription, and the 
-- Quantity tag for all the prescriptions issued by 'Ally Scripts'.

select pr.prescriptionid, sum(co.quantity) total_quantity, 
(case when sum(co.quantity) < 20 then "low quantity"
when count(co.quantity) <= 49 then "medium quantity"                                                   
when count(co.quantity) >= 50 then "high quantity"
end) quantity_tag
from prescription pr join contain co on co.prescriptionid = pr.prescriptionid
join pharmacy ph on ph.pharmacyid = pr.pharmacyid where ph.pharmacyname = "ally scripts" group by pr.prescriptionid;

-- Problem Statement 3: In the Inventory of a pharmacy 'Spot Rx' the quantity of medicine is considered 
-- ‘HIGH QUANTITY’ when the quantity exceeds 7500 and ‘LOW QUANTITY’ when the quantity falls short of 
-- 1000. The discount is considered “HIGH” if the discount rate on a product is 30% or higher, and the 
-- discount is considered “NONE” when the discount rate on a product is 0%.
-- 'Spot Rx' needs to find all the Low quantity products with high discounts and all the high-quantity 
-- products with no discount so they can adjust the discount rate according to the demand. 
-- Write a query for the pharmacy listing all the necessary details relevant to the given requirement.

with cte as (
select medicineID,quantity,discount, 
(case when quantity <=1000 then "Low quantity"
	  when quantity>= 7500 then "High quantity"   
end ) as quantity_tag ,
(case when discount<=0 then "None"
	  when discount>=30 then "High"
end ) as discount_tag
from keep k join pharmacy ph on k.pharmacyID=ph.pharmacyID 
where pharmacyName="Spot Rx" )
select * from cte where (quantity_tag='High quantity' and discount_tag='None') or (quantity_tag='Low quantity' and discount_tag='High') 
order by quantity_tag;

-- Problem Statement 4: Mack, From HealthDirect Pharmacy, wants to get a list of all the affordable and
-- costly, hospital-exclusive medicines in the database. Where affordable medicines are the medicines
-- that have a maximum price of less than 50% of the avg maximum price of all the medicines in the
-- database, and costly medicines are the medicines that have a maximum price of more than double the
-- avg maximum price of all the medicines in the database.  Mack wants clear text next to each medicine
-- name to be displayed that identifies the medicine as affordable or costly. The medicines that do not
-- fall under either of the two categories need not be displayed.
-- Write a SQL query for Mack for this requirement.

select * from (select medicineID,productName, maxPrice,
(case
	when maxPrice>2* (select avg(maxPrice) from medicine) then "costly"
    when maxPrice< 0.5*(select avg(maxPrice) from medicine) then "Affordable"
end ) as "medicine_tag"
 from medicine where hospitalExclusive='S') sub where medicine_tag="costly" or medicine_tag="Affordable" order by medicineID;

-- Problem Statement 5:  
-- The healthcare department wants to categorize the patients into the following category.
-- YoungMale: Born on or after 1st Jan  2005  and gender male.
-- YoungFemale: Born on or after 1st Jan  2005  and gender female.
-- AdultMale: Born before 1st Jan 2005 but on or after 1st Jan 1985 and gender male.
-- AdultFemale: Born before 1st Jan 2005 but on or after 1st Jan 1985 and gender female.
-- MidAgeMale: Born before 1st Jan 1985 but on or after 1st Jan 1970 and gender male.
-- MidAgeFemale: Born before 1st Jan 1985 but on or after 1st Jan 1970 and gender female.
-- ElderMale: Born before 1st Jan 1970, and gender male.
-- ElderFemale: Born before 1st Jan 1970, and gender female.
-- Write a SQL query to list all the patient name, gender, dob, and their category.

select pa.patientid, pe.personname, pa.dob, pe.gender, 
(case when pa.dob < "1970-01-01" and pe.gender = "male" then "Elder Male"
	  when pa.dob < "1970-01-01" and pe.gender = "female" then "Elder FeMale"
      when pa.dob < "1985-01-01" and pe.gender = "male" then "Mid Age Male"
      when pa.dob < "1985-01-01" and pe.gender = "female" then "Mid Age Female"
      when pa.dob < "2005-01-01" and pe.gender = "male" then "Adult Male"
      when pa.dob < "2005-01-01" and pe.gender = "female" then "Adult Female"
      when pa.dob >= "2005-01-01" and pe.gender = "male" then "Young Male"
      when pa.dob >= "2005-01-01" and pe.gender = "female" then "Young Female"
end) "category"  from patient pa join person pe on pe.personid = pa.patientid;

#------------------------------------------GROUPING----------------------------------------------------------------#
-- Problem Statement 1: Johansson is trying to prepare a report on patients who have gone through
-- treatments more than once. Help Johansson prepare a report that shows the patient's name,
-- the number of treatments they have undergone, and their age, Sort the data in a way that the
-- patients who have undergone more treatments appear on top.

select count(treatmentid) as number_of_treatment,p2.personname,TIMESTAMPDIFF(YEAR, p1.dob, CURRENT_DATE)
AS age from treatment join patient p1 using(patientid) join person p2 on p1.patientid=p2.personid
group by patientid having count(treatmentid)>1 order by number_of_treatment desc;

-- Problem Statement 2:  Bharat is researching the impact of gender on different diseases, He wants to
-- analyze if a certain disease is more likely to infect a certain gender or not.
-- Help Bharat analyze this by creating a report showing for every disease how many males and females
-- underwent treatment for each in the year 2021. It would also be helpful for Bharat if the
-- male-to-female ratio is also shown.
with cte as (
SELECT diseaseID,diseaseName,
       SUM(CASE WHEN gender = 'male' THEN 1 ELSE 0 END) AS male_count,
       SUM(CASE WHEN gender = 'female' THEN 1 ELSE 0 END) AS female_count,
       (SUM(CASE WHEN gender = 'male' THEN 1 ELSE 0 END) / 
        SUM(CASE WHEN gender = 'female' THEN 1 ELSE 0 END)) AS male_to_female_ratio
from (select t.diseaseID,d.diseasename,gender from disease d join treatment t using(diseaseid) join patient p using(patientid) join person p1 on p.patientid=p1.personid where year(t.date) in ('2021'))as new
group by diseaseid,diseasename order by diseaseid)
select *
from cte
order by diseaseid;

-- Problem Statement 3:  Kelly, from the Fortis Hospital management, has requested a report that shows
-- for each disease, the top 3 cities that had the most number treatment for that disease.
-- Generate a report for Kelly’s requirement.
with cte as
(select 
	d.diseaseName
	,a.city
	,count(t.treatmentID) as treatment_cnt
	,rank() over(partition by d.diseaseName order by count(t.treatmentID) desc) as rnk
from Disease d
join Treatment t on t.diseaseID = d.diseaseID
join Patient p on p.patientID = t.patientID
join Person pn on pn.personID = p.patientID
join Address a on a.addressID = pn.addressID
group by d.diseaseName, a.city)
select 
	diseaseName
	,city
	,treatment_cnt
from cte
where rnk <= 3
order by diseaseName, rnk;


-- Problem Statement 4: Brooke is trying to figure out if patients with a particular disease are
-- preferring some pharmacies over others or not, For this purpose, she has requested a detailed
-- pharmacy report that shows each pharmacy name, and how many prescriptions they have prescribed for
-- each disease in 2021 and 2022, She expects the number of prescriptions prescribed in 2021 and 2022
-- be displayed in two separate columns.
-- Write a query for Brooke’s requirement.

select
	s.pharmacyName
	,d.diseaseName
	-- ,year(t.date) as yr
	,
    SUM(CASE WHEN year(t.date) = '2021' THEN 1 ELSE 0 END) AS 2021count,
	SUM(CASE WHEN year(t.date) = '2022' THEN 1 ELSE 0 END) AS 2022count
from Disease d
join Treatment t on t.diseaseID = d.diseaseID
join Prescription p on p.treatmentID = t.treatmentID
join Pharmacy s on s.pharmacyID = p.pharmacyID
where year(t.date) in (2021, 2022)
group by pharmacyName, d.diseaseName;

-- Problem Statement 5:  Walde, from Rock tower insurance, has sent a requirement for a report that
-- presents which insurance company is targeting the patients of which state the most. 
-- Write a query for Walde that fulfills the requirement of Walde.
-- Note: We can assume that the insurance company is targeting a region more if the patients of that
-- region are claiming more insurance of that company.

with cte as
(select
	ic.companyName
	,a.state
	,count(p.patientID) as patient_cnt
	,rank() over(partition by ic.companyName order by count(p.patientID) desc) as rnk
from InsuranceCompany ic
join InsurancePlan ip on ip.companyID = ic.companyID
join Claim c on c.uin = ip.uin
join Treatment t on t.claimID = c.claimID
join Patient p on p.patientID = t.patientID
join Person pn on pn.personID = p.patientID
join Address a on a.addressID = pn.addressID
group by ic.companyName, a.state)
select
	*
from cte
where rnk = 1;
#--------------------------------------------GROUPING------------------------------------------------------------#
-- Problem Statement 1: 
-- The healthcare department wants a pharmacy report on the percentage of hospital-exclusive medicine prescribed in the year 2022.
-- Assist the healthcare department to view for each pharmacy, the pharmacy id, pharmacy name, total quantity of medicine prescribed in 2022, total quantity of hospital-exclusive medicine prescribed by the pharmacy in 2022, and the percentage of hospital-exclusive medicine to the total medicine prescribed in 2022.
-- Order the result in descending order of the percentage found. 
select *, (hospital_exclusive*1.0/total_quantity_2022)*100 percentage_hospitalexclusive_totalquant
from
(
select p.pharmacyID, s.pharmacyName,sum(quantity) as total_quantity_2022, 
sum(case when hospitalExclusive = 'S' then quantity else 0 end) as hospital_exclusive
from Pharmacy s
join Prescription p on s.pharmacyID = p.pharmacyID
join Treatment t on t.treatmentID = p.treatmentID
join Contain c on c.prescriptionID = p.prescriptionID
join Medicine m on m.medicineID = c.medicineID
where year(t.date) = 2021
group by s.pharmacyID,s.pharmacyName
) s
order by pharmacyID;

-- Problem Statement 2:  
-- Sarah, from the healthcare department, has noticed many people do not claim insurance for their treatment. She has requested a state-wise report of the percentage of treatments that took place without claiming insurance. Assist Sarah by creating a report as per her requirement.
select
	a.state
	,count(c.claimID) as claim_cnt
	,count(t.treatmentID) as treat_cnt
	,100 - ( count(c.claimID) * 100.0/ count(t.treatmentID) ) as no_claim_per_total_treatment
from Treatment t
join Patient p on p.patientID = t.patientID
join Person pers on pers.personID = p.patientID
left join Claim c on c.claimID = t.claimID
join Address a on a.addressID = pers.addressID
group by a.state;


-- Problem Statement 3:  
-- Sarah, from the healthcare department, is trying to understand if some diseases are spreading
-- in a particular region. Assist Sarah by creating a report which shows for each state, the number of
-- the most and least treated diseases by the patients of that state in the year 2022. 
with cte as
(select 
	a.state
	,d.diseaseName
	,count(p.patientID) as dis_cnt
	,rank() over(partition by a.state order by count(p.patientID) desc) as max_rnk
	,rank() over(partition by a.state order by count(p.patientID) ) as min_rnk
from Disease d 
join Treatment t on t.diseaseID = d.diseaseID
join Patient p on p.patientID = t.patientID
join Person pn on pn.personID = p.patientID
join Address a on a.addressID = pn.addressID
where year(t.date) = 2022
group by a.state, d.diseaseName)
select 
	c1.state
	,c1.diseaseName as most_treated_disease
	,c2.diseaseName as least_treated_disease
from cte c1
join cte c2 on c1.state = c2.state
where c1.max_rnk = 1 and c2.min_rnk = 1;

with cte as (
select a.state ,t.diseaseID  , count(t.diseaseID) as counts
, row_number() over (partition by a.state order by count(t.diseaseID) desc) as r1,
 row_number() over (partition by a.state order by count(t.diseaseID) asc) as r2
from 
address a join person p using(addressID) 
join treatment t on t.patientID = p.personID 
where year(date) = 2022
group by a.state,t.diseaseID)
select state,diseaseid,diseasename,counts ,case when r1=1 then "highest_effected" 
else "least_effected" end as category  from cte
join disease using(diseaseid)
where r1=1 or r2=1 order by 1,2;


-- Problem Statement 4: 
-- Manish, from the healthcare department, wants to know how many registered people are registered as patients as well, in each city. Generate a report that shows each city that has 10 or more registered people belonging to it and the number of patients from that city as well as the percentage of the patient with respect to the registered people.
select
	a.city
	,count(pn.personID) as pers_cnt
	,count(p.patientID) as pat_cnt
	,count(p.patientID) * 100.0 / count(pn.personID) as pat_per_ration
from Person pn
left join Patient p on pn.personID = p.patientID
join Address a on a.addressID = pn.addressID
group by a.city
having count(pn.personID) > 10;


-- Problem Statement 5:  
-- It is suspected by healthcare research department that the substance “ranitidine” might be causing some side effects. Find the top 3 companies using the substance in their medicine so that they can be informed about it.
select
	p.pharmacyName
	,sum(k.quantity) as med_cnt
from Medicine m
join Keep k on k.medicineID = m.medicineID
join Pharmacy p on p.pharmacyID = k.pharmacyID
where m.substanceName like '%ranitidin%'
group by p.pharmacyName
order by med_cnt desc
limit 3;

#-----------------------------------------------IF-THEN-ELSE---------------------------------------------------------#
-- Problem Statement 1: 
-- Insurance companies want to know if a disease is claimed higher or lower than average.  Write a stored procedure that returns “claimed higher than average” or “claimed lower than average” when the diseaseID is passed to it. 
-- Hint: Find average number of insurance claims for all the diseases.  If the number of claims for the passed disease is higher than the average return “claimed higher than average” otherwise “claimed lower than average”.
delimiter //
create procedure typeClaim(in id int)
begin
with cte as (
select avg(cnt) as totalAvg from (
select count(c.claimid) as cnt from disease d join treatment t using(diseaseid) join claim c 
using (claimid) group by d.diseaseid) as derived
) 
select d.diseaseid ,d.diseaseName,count(c.claimid),
case when count(c.claimid)> cte.totalAvg then "claimed higher than average"
 else "claimed lower than average" 
 
end as claimType
from disease d join treatment t using(diseaseid) join claim c  using (claimid)
  join cte  group by d.diseaseid,cte.totalAvg having d.diseaseid=id ; 

end //

delimiter ;

call typeClaim(8);

-- Problem Statement 2:  
/*Joseph from Healthcare department has requested for an application which helps him get genderwise report for any disease. 
Write a stored procedure when passed a disease_id returns 4 columns,
disease_name, number_of_male_treated, number_of_female_treated, more_treated_gender
Where, more_treated_gender is either ‘male’ or ‘female’ based on which gender underwent more often for the disease, if the number is same for both the genders, the value should be ‘same’.
*/
select *,case
 when number_of_male_treated>number_of_female_treated then 'male'
 when number_of_male_treated<number_of_female_treated then 'female'
 else  'same' end as more_treated_gender from(
select d.diseaseName,sum(case when p.gender="male" then 1 else 0 end)as number_of_male_treated,
sum(case when p.gender="female" then 1 else 0 end)as number_of_female_treated from person p join 
treatment t on t.patientID=p.personID join disease d using (diseaseid) group by d.diseaseid
) as derived;


-- Problem Statement 3:  
/*The insurance companies want a report on the claims of different insurance plans. 
Write a query that finds the top 3 most and top 3 least claimed insurance plans.
The query is expected to return the insurance plan name, the insurance company name which has that plan, and whether the plan is the most claimed or least claimed. 
*/
with cte as (

select ip.planName,ic.companyName,count(c.claimID) as total_claims,
dense_rank() over (order by count(c.claimID) desc) as rank_max,
dense_rank() over (order by count(c.claimID) asc) as rank_min

from InsuranceCompany ic
join InsurancePlan ip on ic.companyID=ip.companyID
join Claim c on c.uin=ip.uin
group by ip.planName,ic.companyName )


select planname,companyname,total_claims,
case 
when rank_min<=3 then 'Least_claimed'
when rank_max<=3 then 'Most_claimed'
end as category
from cte 
where rank_max<=3 or rank_min<=3
order by total_claims desc;


-- Problem Statement 4: 
/*The healthcare department wants to know which category of patients is being affected the most by each disease.
Assist the department in creating a report regarding this.
Provided the healthcare department has categorized the patients into the following category.
YoungMale: Born on or after 1st Jan  2005  and gender male.
YoungFemale: Born on or after 1st Jan  2005  and gender female.
AdultMale: Born before 1st Jan 2005 but on or after 1st Jan 1985 and gender male.
AdultFemale: Born before 1st Jan 2005 but on or after 1st Jan 1985 and gender female.
MidAgeMale: Born before 1st Jan 1985 but on or after 1st Jan 1970 and gender male.
MidAgeFemale: Born before 1st Jan 1985 but on or after 1st Jan 1970 and gender female.
ElderMale: Born before 1st Jan 1970, and gender male.
ElderFemale: Born before 1st Jan 1970, and gender female.
*/
with cte as
(select
	CASE
		WHEN pt.dob >= '2005-01-01' AND gender = 'Male' THEN 'YoungMale'
		WHEN pt.dob >= '2005-01-01' AND gender = 'Female' THEN 'YoungFemale'
		WHEN pt.dob < '2005-01-01' AND pt.dob >= '1985-01-01' AND gender = 'Male' THEN 'AdultMale'
		WHEN pt.dob < '2005-01-01' AND pt.dob >= '1985-01-01' AND gender = 'Female' THEN 'AdultFemale'
		WHEN pt.dob < '1985-01-01' AND pt.dob >= '1970-01-01' AND gender = 'Male' THEN 'MidAgeMale'
		WHEN pt.dob < '1985-01-01' AND pt.dob >= '1970-01-01' AND gender = 'Female' THEN 'MidAgeFemale'
		WHEN pt.dob < '1970-01-01' AND gender = 'Male' THEN 'ElderMale'
		WHEN pt.dob < '1970-01-01' AND gender = 'Female' THEN 'ElderFemale'
		ELSE 'Unknown'
	END as category
	,d.diseaseName
from Patient pt
join Person p on p.personID = pt.patientID
join Treatment t on t.patientID = pt.patientID
join Disease d on d.diseaseID = t.diseaseID
)
,cte1 as
(select 
	category
	,diseaseName
	,count(diseaseName) as dis_per_cat
	,row_number() over(partition by diseaseName order by count(diseaseName) desc) as rnk
from cte
group by diseaseName, category)
select 
	diseaseName
	,category
	,dis_per_cat
from cte1
where rnk = 1;


-- Problem Statement 5:  
/*Anna wants a report on the pricing of the medicine. She wants a list of the most expensive and most affordable medicines only. 
Assist anna by creating a report of all the medicines which are pricey and affordable, listing the companyName, productName, description, maxPrice, and the price category of each. Sort the list in descending order of the maxPrice.
Note: A medicine is considered to be “pricey” if the max price exceeds 1000 and “affordable” if the price is under 5. Write a query to find 
*/
with cte as
(select
	p.pharmacyName
	,m.productName
	,m.description
	,m.maxPrice
	,case
		when  maxPrice > 1000 then 'priecy'
		when  maxPrice < 5 then 'affordable'
		else 'mid'
	end as product_category
from Pharmacy p
join Keep k on k.pharmacyID = p.pharmacyID
join Medicine m on m.medicineID = k.medicineID)
select *
from cte
where product_category != 'mid'
order by maxPrice desc;
#-------------------------------------------------QUERY OPTIMIZATION---------------------------------------------------#
-- Problem 1
-- For each age(in years), how many patients have gone for treatment?

-- Optimized Query
-- Changed * to patientID
-- Ans 1
-- Using YEAR in DATEDIFF instead of hour
SELECT 
	DATEDIFF(year, dob , GETDATE()) AS age, 
	count(Treatment.patientID) AS numTreatments
FROM Person
JOIN Patient ON Patient.patientID = Person.personID
JOIN Treatment ON Treatment.patientID = Patient.patientID
group by DATEDIFF(year, dob , GETDATE())
order by numTreatments desc;

-- Ans 2
-- Tried to avoid date func in groupBy
with cte as
(SELECT 
	DATEDIFF(year, dob , GETDATE()) AS age, 
	1 as cnt
FROM Person
JOIN Patient ON Patient.patientID = Person.personID
JOIN Treatment ON Treatment.patientID = Patient.patientID
)
select
	age
	,sum(cnt) AS numTreatments
from cte
group by age
order by numTreatments desc;

-- Problem 2
-- For each city, Find the number of registered people, number of pharmacies, and number of insurance companies.

SELECT
    A.city,
    COUNT(P.pharmacyID) AS numPharmacy,
    COUNT(I.companyID) AS numInsuranceCompany,
    COUNT(Pe.personID) AS numRegisteredPeople
FROM
    Address A
LEFT JOIN
    Pharmacy P ON P.addressID = A.addressID
LEFT JOIN
    InsuranceCompany I ON I.addressID = A.addressID
LEFT JOIN
    Person Pe ON Pe.addressID = A.addressID
GROUP BY
    A.city
ORDER BY
    numRegisteredPeople DESC;
    
-- Problem 3
-- Total quantity of medicine for each prescription prescribed by Ally Scripts
-- If the total quantity of medicine is less than 20 tag it as "Low Quantity".
-- If the total quantity of medicine is from 20 to 49 (both numbers including) tag it as "Medium Quantity".
-- If the quantity is more than equal to 50 then tag it as "High quantity".
-- In previous we are using sum in very case statement multiple time, hence tried to minimize use of CASE statement
with cte as
(select 
	P.prescriptionID, 
	sum(quantity) as totalQuantity
FROM Contain C
JOIN Prescription P on P.prescriptionID = C.prescriptionID
JOIN Pharmacy on Pharmacy.pharmacyID = P.pharmacyID
where Pharmacy.pharmacyName = 'Ally Scripts'
group by P.prescriptionID)
select
	prescriptionID
	,totalQuantity
	,CASE 
		WHEN totalQuantity < 20 THEN 'Low Quantity'
		WHEN totalQuantity < 50 THEN 'Medium Quantity'
		ELSE 'High Quantity' 
	END AS Tag
from cte;

-- Problem 4
-- The total quantity of medicine in a prescription is the sum of the quantity of all the medicines in the prescription.
-- Select the prescriptions for which the total quantity of medicine exceeds
-- the avg of the total quantity of medicines for all the prescriptions.

-- The question aked is Prescription who exceed avg medicine quantity in single prescripotion
-- So I neglected Pharmacy join and used CTE and avoid creating a new table
with cte as
(select  Prescription.prescriptionID, sum(quantity) as totalQuantity
from Prescription
join Contain on Contain.prescriptionID = Prescription.prescriptionID
join Medicine on Medicine.medicineID = Contain.medicineID
join Treatment on Treatment.treatmentID = Prescription.treatmentID
where YEAR(date) = 2022
group by Prescription.prescriptionID)
select
	prescriptionID
	,totalQuantity
from cte
where totalQuantity > (select avg(totalQuantity) from cte);

-- Problem 5

-- Select every disease that has 'p' in its name, and 
-- the number of times an insurance claim was made for each of them. 

-- To count Claim ID we only need Treatment table
-- Joining Disease table for name check, and changed the where condition by removing sub-query

SELECT Disease.diseaseName, COUNT(Treatment.claimID) as numClaims
FROM Disease
JOIN Treatment ON Disease.diseaseID = Treatment.diseaseID
WHERE diseaseName LIKE '%p%'
GROUP BY diseaseName;

#--------------------------------------rollup--------------------------------------------------------------#
/*
Brian, the healthcare department, has requested for a report that shows for each state how many people 
underwent treatment for the disease “Autism”.  He expects the report to show the data for each state as well as each gender and 
for each state and gender combination. 
Prepare a report for Brian for his requirement.
*/
select state,gender,count(patientid) as patient_count from address a 
join person p using(addressid)
join treatment t on t.patientid=p.personid
join disease d using (diseaseid)
where diseasename='Autism'
group by state,gender with rollup;

/*
Problem statement-2
Insurance companies want to evaluate the performance of different insurance plans they offer. 
Generate a report that shows each insurance plan, the company that issues the plan, and 
the number of treatments the plan was claimed for. The report would be more relevant if the data compares the performance 
for different years(2020, 2021 and 2022) and if the report also includes the total number of claims in the different years,
 as well as the total number of claims for each plan in all 3 years combined.
*/
select companyname,planname,year(date) as year,count(patientid) as patient_count from insurancecompany ic
join insuranceplan ip using(companyid)
join claim c using(uin)
join treatment t using(claimid)
where year(date) in (2020,2021,2022)
group by companyname,planname,year(date) with rollup;
/*
Problem Statement 3:  
Sarah, from the healthcare department, is trying to understand if some diseases are spreading in a particular region. 
Assist Sarah by creating a report which shows each state the number of the most and 
least treated diseases by the patients of that state in the year 2022. 
It would be helpful for Sarah if the aggregation for the different combinations is found as well. Assist Sarah to create this report. 

*/
select state,diseasename,count(patientid) as patient_count  from address a
join person p using(addressid)
join treatment t on t.patientid=p.personid
join disease d using(diseaseid)
where year(date)=2022
group by state,diseasename with rollup;
/*
Problem Statement 4: 
Jackson has requested a detailed pharmacy report that shows each pharmacy name, and how many prescriptions
 they have prescribed for each disease in the year 2022, along with this Jackson also needs to view 
 how many prescriptions were prescribed by each pharmacy, and the total number prescriptions were prescribed for each disease.
Assist Jackson to create this report. 
*/
select pharmacyname,diseasename,count(prescriptionid) as prescription_count  from pharmacy p 
join prescription pr using(pharmacyid)
join treatment t using(treatmentid)
join disease d using(diseaseid)
where year(date)=2022
group by pharmacyname,diseasename with rollup;
/*
Problem Statement 5:  
Praveen has requested for a report that finds for every disease how many males and females underwent treatment for each in the year 2022.
 It would be helpful for Praveen if the aggregation for the different combinations is found as well.
Assist Praveen to create this report. 
*/
select diseasename,case when gender='male' then 'Male' else 'Female' end as gender_category,count(patientid) as patient_count
 from person p 
join treatment t on p.personID = t.patientid
join disease d using (diseaseid)
group by diseasename,gender_category with rollup;

#-------------------------------------stored routines-------------------------------------------------------------------#
/*Problem Statement 1:
The healthcare department has requested a system to analyze the performance of insurance companies and their plan.
For this purpose, create a stored procedure that returns the performance of different insurance plans of an insurance company.
 When passed the insurance company ID the procedure should generate and return all the insurance plan names the provided company issues, 
 the number of treatments the plan was claimed for, and the name of the disease the plan was claimed for the most. 
 The plans which are claimed more are expected to appear above the plans that are claimed less.
*/
/*with insurance_cte as (select companyid,planname, diseasename,treatmentid from insuranceplan ip
join claim c using(uin)
join treatment t using (claimid)
join disease d using (diseaseid)
where ip.companyid=7923
order by planname),
total_treat_count as (select planname,count(treatmentid) from insurance_cte group by planname),
treat_count as (select planname,diseasename,count(treatmentid),rank() over(partition by planname order by count(treatmentid) desc) as rk from insurance_cte group by planname,diseasename)
select * from total_treat_count join treat_count using (planname) where rk=1;*/
delimiter //
create procedure insurance_cte(in company_id int)
begin
select planname,total_treatments,diseasename,treatments_disease from 
(select planname,count(treatmentid) as total_treatments from 
	(select companyid,planname, diseasename,treatmentid from insuranceplan ip
	join claim c using(uin)
	join treatment t using (claimid)
	join disease d using (diseaseid)
	where ip.companyid=company_id
	order by planname) p group by planname) d1
 join 
 (select planname,diseasename,count(treatmentid) as treatments_disease,rank() over(partition by planname order by count(treatmentid) desc) as rk from 
	(select companyid,planname, diseasename,treatmentid from insuranceplan ip
	join claim c using(uin)
	join treatment t using (claimid)
	join disease d using (diseaseid)
	where ip.companyid=company_id
	order by planname) q group by planname,diseasename) d2
 using (planname) where rk=1;
end //
delimiter ;
drop procedure insurance_cte;
call insurance_cte(7923);


/*
Problem Statement 2:
It was reported by some unverified sources that some pharmacies are more popular for certain diseases. 
The healthcare department wants to check the validity of this report.
Create a stored procedure that takes a disease name as a parameter and would return the top 3 pharmacies the patients are preferring
 for the treatment of that disease in 2021 as well as for 2022.
Check if there are common pharmacies in the top 3 list for a disease, in the years 2021 and the year 2022.
Call the stored procedure by passing the values “Asthma” and “Psoriasis” as disease names and draw a conclusion from the result.
*/
delimiter //
create procedure top_pharmacies(in disease_name varchar(30))
begin
select diseasename,pharmacyname,count(patientid) as count from (select * from disease where diseasename=disease_name) d 
join treatment t using(diseaseid)
join prescription pr using(treatmentid)
join pharmacy p using(pharmacyid) where year(date) in (2021,2022)
group by diseasename,pharmacyname
order by diseasename,count desc limit 3;
end //
delimiter ;
drop procedure top_pharmacies;
call top_pharmacies('Asthma');
call top_pharmacies('Psoriasis');

/*
Problem Statement 3:
Jacob, as a business strategist, wants to figure out if a state is appropriate for setting up an insurance company or not.
Write a stored procedure that finds the num_patients, num_insurance_companies, and insurance_patient_ratio, 
the stored procedure should also 
find the avg_insurance_patient_ratio and if the insurance_patient_ratio of the given state is less than the avg_insurance_patient_ratio 
then it Recommendation section can have the value “Recommended” otherwise the value can be “Not Recommended”.
Description of the terms used:
num_patients: number of registered patients in the given state
num_insurance_companies:  The number of registered insurance companies in the given state
insurance_patient_ratio: The ratio of registered patients and the number of insurance companies in the given state
avg_insurance_patient_ratio: The average of the ratio of registered patients and the number of insurance for all the states.
*/
delimiter //
create procedure set_company(in state_name varchar(3))
begin
declare patient_count int;
declare company_count int;
declare insurance_patient_ratio decimal(7,3);
declare avg_insurance_patient_ratio decimal(7,3);
select count(patientid) into patient_count from treatment t
join person p on t.patientid=p.personid 
join address a using (addressid)
where state=state_name;

select count(companyid) into company_count from address a 
join insurancecompany ip using (addressid);
set insurance_patient_ratio=if((company_count)>0,(patient_count/company_count),0);
select avg(patient_count/company_count) into avg_insurance_patient_ratio
	from
	(select state,count(patientid) as patient_count
	from treatment t
	join person p on t.patientid=p.personid
	join address a using (addressid)
	group by state) t1 
	join 
	(select state,count(companyid) as company_count from insurancecompany ip 
	join address a using (addressid) 
	group by state) t2 using (state);
if insurance_patient_ratio<avg_insurance_patient_ratio then 
	select state_name,patient_count,company_count,insurance_patient_ratio, 'recommended' as recommended_section;
else 
	select state_name,patient_count,company_count,insurance_patient_ratio, 'Not recommended' as recommended_section;
end if;
end//
delimiter ;
drop procedure set_company;
call set_company('AZ');


/*
Problem Statement 4:
Currently, the data from every state is not in the database, The management has decided to add the data from other states and cities as well. It is felt by the management that it would be helpful if the date and time were to be stored whenever new city or state data is inserted.
The management has sent a requirement to create a PlacesAdded table if it doesn’t already exist, that has four attributes. placeID, placeName, placeType, and timeAdded.
Description
placeID: This is the primary key, it should be auto-incremented starting from 1
placeName: This is the name of the place which is added for the first time
placeType: This is the type of place that is added for the first time. The value can either be ‘city’ or ‘state’
timeAdded: This is the date and time when the new place is added

You have been given the responsibility to create a system that satisfies the requirements of the management. Whenever some data is inserted in the Address table that has a new city or state name, the PlacesAdded table should be updated with relevant data. 
*/
create table if not exists PlacesAdded(
	placeID int primary key auto_increment,
	placeName varchar(50),
	placType enum('city', 'state'),
	timeAdded timestamp default now()
);
/*
Problem Statement 5:
Some pharmacies suspect there is some discrepancy in their inventory management. The quantity in the ‘Keep’ is updated regularly and there is no record of it. They have requested to create a system that keeps track of all the transactions whenever the quantity of the inventory is updated.
You have been given the responsibility to create a system that automatically updates a Keep_Log table which has  the following fields:
id: It is a unique field that starts with 1 and increments by 1 for each new entry
medicineID: It is the medicineID of the medicine for which the quantity is updated.
quantity: The quantity of medicine which is to be added. If the quantity is reduced then the number can be negative.
For example:  If in Keep the old quantity was 700 and the new quantity to be updated is 1000, then in Keep_Log the quantity should be 300.
Example 2: If in Keep the old quantity was 700 and the new quantity to be updated is 100, then in Keep_Log the quantity should be -600.
*/
delimiter //
CREATE TRIGGER update_keep_log
AFTER UPDATE ON Keep
FOR EACH ROW
BEGIN
    INSERT INTO Keep_Log (medicineID, quantity) VALUES (NEW.medicineID, NEW.quantity - OLD.quantity);
END//
delimiter ;


#---------------------------------------------STORED ROUTINES-----------------------------------------------------------#
/*Problem Statement 1:
Problem Statement 1:
Patients are complaining that it is often difficult to find some medicines. 
They move from pharmacy to pharmacy to get the required medicine. A system is required that finds the pharmacies and 
their contact number that have the required medicine in their inventory. 
So that the patients can contact the pharmacy and order the required medicine.
Create a stored procedure that can fix the issue.
*/ #ostenan
delimiter //
create procedure pharmacy_list(in medicine_name varchar(30))
begin
select pharmacyname,phone from medicine m 
join keep k using(medicineid)
join pharmacy p using (pharmacyid)
where productname=medicine_name;
end //
delimiter ;
call pharmacy_list('ostenan')
/*
Problem Statement 2:
The pharmacies are trying to estimate the average cost of all the prescribed medicines per prescription,
 for all the prescriptions they have prescribed in a particular year.
 Create a stored function that will return the required value when the pharmacyID and year are passed to it. 
 Test the function with multiple values.
*/#1008, 2021
delimiter //
create function average_cost(pharmacy_id int,year1 int)
returns decimal(15,3)
deterministic
begin
declare avg_max_Price decimal(15,3);
declare p_count int;
select avg(average) into avg_max_Price  from (
select prescriptionid, avg(maxPrice) as average
from medicine m join contain c using(medicineid)
join prescription pr using (prescriptionid)
join treatment t using (treatmentid)
where pharmacyid=pharmacy_id and year(date)=(year1)
group by prescriptionid) as temp ;
return avg_max_Price;
end //
delimiter ;
drop function average_cost;
select average_cost(1008,'2021');
/*
delimiter //
create procedure average_cost(in pharmacy_id int, in year1 int)
begin
select prescriptionid,avg(maxPrice) as avg_max_Price from medicine m join contain c using (medicineid)
join prescription pr using (prescriptionid)
join treatment t using (treatmentid)
where pharmacyid=pharmacy_id and year(date)=year1
group by prescriptionid;
end //
delimiter ;
drop procedure average_cost;
call average_cost(1008,2022);
select pharmacyid from keep;
*/
/*
Problem Statement 3:
The healthcare department has requested an application that finds out the disease that was spread the most in a state for a given year.
 So that they can use the information to compare the historical data and gain some insight.
Create a stored function that returns the name of the disease for which the patients from a particular state had
 the most number of treatments for a particular year. Provided the name of the state and year is passed to the stored function.
*/
delimiter //
create function disease_count_year( state_name varchar(30), year1 year) returns varchar(30)
deterministic
begin
declare disease_name varchar(30);
select diseasename into disease_name from
(select diseasename,state,count(treatmentid) as count , rank() over (order by count(treatmentid) desc) as rk from disease d 
join treatment t using (diseaseid)
join person p on p.personid=t.patientid
join address a using (addressid)
where state=state_name and year(date)=year1
group by diseasename,state) temp where rk=1;
return disease_name;
end//
delimiter ;
drop function disease_count_year;
select disease_count_year('CO',2022) as disease_name;
/*
-- stored procedure
delimiter //
create procedure disease_count (in state_name varchar(30),in year int)
begin
select diseasename,state,count from
(select diseasename,state,count(treatmentid) as count , rank() over (order by count(treatmentid) desc) as rk from disease d 
join treatment t using (diseaseid)
join person p on p.personid=t.patientid
join address a using (addressid)
where state=state_name and year(date)=year
group by diseasename,state) temp where rk=1;
end //
delimiter ;
drop procedure disease_count;
call disease_count('CO',2022);
*/
/*
Problem Statement 4:
The representative of the pharma union, Aubrey, has requested a system that she can use to 
find how many people in a specific city have been treated for a specific disease in a specific year.
Create a stored function for this purpose.
*/ #Edmond	Metabolic syndrome	2
delimiter //
create function count_of_disease(year int,city_name varchar(20),disease_name varchar(30)) returns int
deterministic
begin
declare patient_count int;
select count(treatmentid) into patient_count from disease d
join treatment t using (diseaseid)
join person p on p.personid=t.patientid
join address a using (addressid)
where year(date)=year and diseasename=disease_name and city=city_name
group by city,diseasename;
return patient_count;
end //
delimiter ;
drop function count_of_disease;
select count_of_disease(2021,'Edmond','Metabolic syndrome') as patient_count ;
/*
-- stored procedure
delimiter //
create procedure city_disease(in year int)
begin
select count(treatmentid) as patient_count from disease d
join treatment t using (diseaseid)
join person p on p.personid=t.patientid
join address a using (addressid)
where year(date)=year -- and city=city_name and diseasename=disease_name
group by city,diseasename;
end //
delimiter ;
call city_disease(2022);
*/
/*
Problem Statement 5:
The representative of the pharma union, Aubrey, is trying to audit different aspects of the pharmacies. 
She has requested a system that can be used to find the average balance for claims submitted by a specific insurance company in  2022. 
Create a stored function that can be used in the requested application. 
*/
delimiter //
create function average_balance(company_name varchar(100))
returns decimal(15,3)
deterministic
begin
declare average decimal(15,3);
select avg(balance) into average from insuranceplan ip 
join insurancecompany ic using(companyid)
join claim c using(uin)
join treatment t using(claimid)
where year(date)=2022 and companyname=company_name
group by companyname;
return average;
end //
delimiter ;
drop function average_balance;
select companyname ,average_balance(companyname)from insurancecompany group by companyname;










