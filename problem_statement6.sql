-- Problem Statement 1: 
-- The healthcare department wants a pharmacy report on the percentage of hospital-exclusive medicine prescribed in the year 2022.
-- Assist the healthcare department to view for each pharmacy, the pharmacy id, pharmacy name, total quantity of medicine prescribed
--  in 2022, total quantity of hospital-exclusive medicine prescribed by the pharmacy in 2022, and the percentage of hospital-exclusive
--  medicine to the total medicine prescribed in 2022.
-- Order the result in descending order of the percentage found. 
SELECT 
    p.pharmacyID,
    pharmacyName,
    COUNT(m.medicineID) AS total_med,
    SUM(IF(hospitalExclusive = 'S', 1, 0)) AS total_ex_med,
    (SUM(IF(hospitalExclusive = 'S', 1, 0))/COUNT(m.medicineID))*100 as ex_med_percentage
FROM
    pharmacy p
        JOIN
    keep k ON k.pharmacyID = p.pharmacyID
        JOIN
    medicine m ON m.medicineID = k.medicineID
        JOIN
    prescription pr ON pr.pharmacyID = p.pharmacyID
        JOIN
    treatment t ON t.treatmentID = pr.treatmentID
WHERE
    YEAR(t.date) = 2022
GROUP BY p.pharmacyID , pharmacyName
order by ex_med_percentage desc;

-- Problem Statement 2:  
-- Sarah, from the healthcare department, has noticed many people do not claim insurance for their treatment. She has requested a
--  state-wise report of the percentage of treatments that took place without claiming insurance. Assist Sarah by creating a report
--  as per her requirement.
SELECT 
    a.state,
    COUNT(t.treatmentID) AS total_treatment,
    COUNT(c.claimID) AS total_claim,
    ((COUNT(t.treatmentID)-COUNT(c.claimID))/COUNT(t.treatmentID))*100 as percentage
FROM
    treatment t
        LEFT JOIN
    claim c USING (claimID)
        JOIN
    person p ON p.personID = t.patientID
        JOIN
    address a ON a.addressID = p.addressID
GROUP BY a.state;

-- Problem Statement 3:  
-- Sarah, from the healthcare department, is trying to understand if some diseases are spreading in a particular region. Assist Sarah
--  by creating a report which shows for each state, the number of the most and least treated diseases by the patients of that state
--  in the year 2022. 
with cte as (select state, diseaseName, count(p.personID) as treatment_count
from treatment t
join disease d using(diseaseID)
join person p on p.personID = t.patientID
join address a using(addressID)
group by state, diseaseName),
cte1 as (
select state, diseaseName, treatment_count,
dense_rank() over (partition by state order by treatment_count desc) as rankk from cte
union
select state, diseaseName, treatment_count,
dense_rank() over (partition by state order by treatment_count asc) as rankk from cte
)
select * from cte1
where rankk = 1
order by state, treatment_count desc;

-- Problem Statement 4: 
-- Manish, from the healthcare department, wants to know how many registered people are registered as patients as well, in each
--  city. Generate a report that shows each city that has 10 or more registered people belonging to it and the number of patients
--  from that city as well as the percentage of the patient with respect to the registered people.
SELECT 
    city,
    COUNT(patientID) AS patient_count,
    COUNT(personID) AS people,
    COUNT(patientID)/COUNT(personID)*100 as percentage
FROM
    person p1
        JOIN
    address a ON a.addressID = p1.addressID
        LEFT JOIN
    patient p2 ON p1.personID = p2.patientID
GROUP BY city
HAVING people >= 10;

-- Problem Statement 5:  
-- It is suspected by healthcare research department that the substance “ranitidine” might be causing some side effects. Find the
--  top 3 companies using the substance in their medicine so that they can be informed about it.
select companyName, count(medicineID) as num_of_meds from medicine
where substanceName like "%ranitidina%"
group by companyName
order by num_of_meds desc
limit 3;
