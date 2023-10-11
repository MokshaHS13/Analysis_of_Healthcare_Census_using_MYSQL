-- Problem Statement 1: 
-- Brian, the healthcare department, has requested for a report that shows for each state how many people underwent
--  treatment for the disease “Autism”.  He expects the report to show the data for each state as well as each gender
--  and for each state and gender combination. 
-- Prepare a report for Brian for his requirement.
SELECT 
    IFNULL(gender, 'gender total') AS gender,
    IFNULL(state, 'state total') AS state,
    COUNT(personID) AS treatment_count
FROM
    treatment t
        JOIN
    person p ON t.patientID = p.personID
        JOIN
    address a ON p.addressID = a.addressID
GROUP BY gender , state WITH ROLLUP;

-- Problem Statement 2:  
-- Insurance companies want to evaluate the performance of different insurance plans they offer. 
-- Generate a report that shows each insurance plan, the company that issues the plan, and the number of treatments the 
-- plan was claimed for. The report would be more relevant if the data compares the performance for different years(2020, 2021 and 2022) 
-- and if the report also includes the total number of claims in the different years, as well as the total number of claims for each 
-- plan in all 3 years combined.
SELECT 
    ic.companyName,
    ip.planName,
    YEAR(t.date),
    COUNT(treatmentID),
    COUNT(c.claimID)
FROM
    treatment t
        JOIN
    claim c ON t.claimID = c.claimID
        JOIN
    insuranceplan ip ON ip.uin = c.uin
        JOIN
    insurancecompany ic ON ic.companyID = ip.companyID
WHERE
    YEAR(t.date) IN (2020 , 2021, 2022)
GROUP BY ic.companyName , ip.planName , t.date WITH ROLLUP;
 
-- Problem Statement 3:  
-- Sarah, from the healthcare department, is trying to understand if some diseases are spreading in a particular region. Assist 
-- Sarah by creating a report which shows each state the number of the most and least treated diseases by the patients of that 
-- state in the year 2022. It would be helpful for Sarah if the aggregation for the different combinations is found as well. 
-- Assist Sarah to create this report. 
with cte as (
	select 
		a.state,
		d.diseaseName,
		count(distinct d.diseaseName) as treat_count,
		dense_rank() over(partition by a.state order by count(*) desc) as most,
		dense_rank() over(partition by a.state order by count(*)) as least
	from 
		disease d 
	join treatment t using (diseaseID)
	join patient p using (patientID)
	join person p2 on p2.personID = p.patientID 
	join address a using (addressID)
	where year(t.`date`) = 2022
	group by a.state, d.diseaseName 
)
select 
	c1.state,
	c1.diseaseName as most_infected,
	c2.diseaseName as least_infected
from cte c1
join cte c2 using(state)
where c1.most = 1 or c2.least = 1;

-- Problem Statement 4: 
-- Jackson has requested a detailed pharmacy report that shows each pharmacy name, and how many prescriptions they have
--  prescribed for each disease in the year 2022, along with this Jackson also needs to view how many prescriptions were
--  prescribed by each pharmacy, and the total number prescriptions were prescribed for each disease.
-- Assist Jackson to create this report. 
SELECT 
    ph.pharmacyName, d.diseaseName, COUNT(pr.prescriptionID)
FROM
    prescription pr
        JOIN
    pharmacy ph ON pr.pharmacyID = ph.pharmacyID
        JOIN
    treatment t ON t.treatmentID = pr.treatmentID
        JOIN
    disease d ON d.diseaseID = t.diseaseID
GROUP BY ph.pharmacyName , d.diseaseName WITH ROLLUP;

-- Problem Statement 5:  
-- Praveen has requested for a report that finds for every disease how many males and females underwent treatment for 
-- each in the year 2022. It would be helpful for Praveen if the aggregation for the different combinations is found as well.
-- Assist Praveen to create this report. 
SELECT 
    d.diseaseName,
    SUM(IF(gender = 'male', 1, 0)) AS male,
    SUM(IF(gender = 'female', 1, 0)) AS female
FROM
    person p
        JOIN
    treatment t ON t.patientID = p.personID
        JOIN
    disease d ON t.diseaseID = d.diseaseID
WHERE
    YEAR(t.date) = 2022
GROUP BY diseaseName WITH ROLLUP;

SELECT 
    d.diseaseName,
    IFNULL(gender, 'Total') AS gender,
    COUNT(treatmentID) AS treat_count
FROM
    person p
        JOIN
    treatment t ON t.patientID = p.personID
        JOIN
    disease d ON t.diseaseID = d.diseaseID
WHERE
    YEAR(t.date) = 2022
GROUP BY diseaseName , gender WITH ROLLUP;
