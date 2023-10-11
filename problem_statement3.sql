-- Problem Statement 1:  Some complaints have been lodged by patients that they have been prescribed hospital-exclusive medicine that 
-- they can’t find elsewhere and facing problems due to that. Joshua, from the pharmacy management, wants to get a report of which
--  pharmacies have prescribed hospital-exclusive medicines the most in the years 2021 and 2022. Assist Joshua to generate the 
--  so that the pharmacies who prescribe hospital-exclusive medicine more often are advised to avoid such practice if possible.   
SELECT 
    p.pharmacyID, p.pharmacyName, m.hospitalExclusive
FROM
    pharmacy p
        JOIN
    keep k ON k.pharmacyID = p.pharmacyID
        JOIN
    medicine m ON k.medicineID = m.medicineID
WHERE
    m.hospitalExclusive = 'S';

-- Problem Statement 2: Insurance companies want to assess the performance of their insurance plans. Generate a report that shows
--  each insurance plan, the company that issues the plan, and the number of treatments the plan was claimed for.
with cte as(
SELECT 
    t.treatmentID, c.claimID, uin
FROM
    treatment t
        JOIN
    claim c USING (claimID)
)
SELECT 
    planName,
    companyName,
    COUNT(c.treatmentID) AS total_treatment
FROM
    insuranceplan ip
        JOIN
    insurancecompany ic ON ip.companyID = ic.companyID
        JOIN
    cte c ON c.uin = ip.uin
GROUP BY planName , companyName;

-- Problem Statement 3: Insurance companies want to assess the performance of their insurance plans. Generate a report that shows
--  each insurance company's name with their most and least claimed insurance plans.
with cte as(
SELECT 
    companyName, planName, COUNT(claimID) AS total_claims
FROM
    claim c
        JOIN
    insuranceplan ip ON ip.uin = c.uin
        JOIN
    insurancecompany ic ON ip.companyID = ic.companyID
GROUP BY companyName , planName),
cte1 as(
select companyName, planName, total_claims,  dense_rank() over (partition by companyName order by total_claims desc) as rankk
from cte
union
select companyName, planName, total_claims,  dense_rank() over (partition by companyName order by total_claims asc) as rankk
from cte)
SELECT 
    *
FROM
    cte1
WHERE
    rankk = 1;

-- Problem Statement 4:  The healthcare department wants a state-wise health report to assess which state requires more attention 
-- in the healthcare sector. Generate a report for them that shows the state name, number of registered people in the state, number
--  of registered patients in the state, and the people-to-patient ratio. sort the data by people-to-patient ratio. 
SELECT 
    a.state,
    COUNT(personID) AS total_person,
    COUNT(patientID) AS total_patient,
    COUNT(personID)/COUNT(patientID) AS ratio
FROM
    patient pa
        RIGHT JOIN
    person pe ON pe.personID = pa.patientID
        JOIN
    address a ON a.addressID = pe.addressID
GROUP BY a.state;

-- Problem Statement 5:  Jhonny, from the finance department of Arizona(AZ), has requested a report that lists the total quantity
--  of medicine each pharmacy in his state has prescribed that falls under Tax criteria I for treatments that took place in 2021.
--  Assist Jhonny in generating the report. 

select ph.pharmacyName, sum(c.quantity) as total_quantity from treatment t
join prescription pr on pr.treatmentID = t.treatmentID
join pharmacy ph on pr.pharmacyID = ph.pharmacyID
join address a on a.addressID = ph.addressID
join contain c on c.prescriptionID = pr.prescriptionID
join medicine m on c.medicineID = m.medicineID 
where m.taxCriteria = 'I' and a.state = 'AZ' and year(t.date) = 2021
group by ph.pharmacyName;