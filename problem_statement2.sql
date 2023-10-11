-- Problem Statement 1: A company needs to set up 3 new pharmacies, they have come up with an idea that the pharmacy can
--  be set up in cities where the pharmacy-to-prescription ratio is the lowest and the number of prescriptions should exceed
--  100. Assist the company to identify those cities where the pharmacy can be set up.
SELECT 
    a.city,
    COUNT(DISTINCT p2.pharmacyID) AS pharma_count,
    COUNT(DISTINCT p1.prescriptionID) AS presc_count,
    COUNT(DISTINCT p1.prescriptionID) / COUNT(DISTINCT p2.pharmacyID) AS ratio
FROM
    prescription p1
        RIGHT JOIN
    pharmacy p2 USING (pharmacyID)
        JOIN
    address a USING (addressID)
GROUP BY a.city
HAVING presc_count > 100
ORDER BY ratio
LIMIT 3;

-- Problem Statement 2: The State of Alabama (AL) is trying to manage its healthcare resources more efficiently. For each city
--  in their state, they need to identify the disease for which the maximum number of patients have gone for treatment. Assist
--  the state for this purpose.
-- Note: The state of Alabama is represented as AL in Address Table.

with cte as 
(select a.state, a.city, d.diseaseName, count(p.personID) as patient_count
from treatment t
join person p on t.patientID = p.personID
join disease d on d.diseaseID = t.diseaseID
join address a on a.addressID = p.addressID
group by a.state, a.city, d.diseaseName, d.diseaseID
having a.state = 'AL'),
cte2 as(
select * from 
(select c.city,
 c.diseaseName,
 c.patient_count,
 dense_rank() over (partition by c.city order by c.patient_count desc) as rankk
 from cte c
 ) c1
where rankk = 1)
select c.city,
 c.diseaseName,
 c.patient_count
 from cte2 c
 order by c.patient_count desc;

-- Problem Statement 3: The healthcare department needs a report about insurance plans. The report is required to include the
--  insurance plan, which was claimed the most and least for each disease.  Assist to create such a report.
with cte as (select diseaseName, planName, count(claimID) as claim_count
from treatment t
join disease d using(diseaseID)
join claim c using(claimID)
join insuranceplan i using(uin)
group by diseaseName, planName),
cte1 as (
select diseaseName, planName, claim_count,
dense_rank() over (partition by diseaseName order by  claim_count desc) as rankk from cte
union
select diseaseName, planName, claim_count,
dense_rank() over (partition by diseaseName order by  claim_count asc) as rankk from cte
)
select * from cte1
where rankk = 1
order by diseaseName;


-- Problem Statement 4: The Healthcare department wants to know which disease is most likely to infect multiple people in the
--  same household. For each disease find the number of households that has more than one patient with the same disease. 
-- Note: 2 people are considered to be in the same household if they have the same address. 

with cte as(
select personID, personName, a.addressID, address1, state, city, diseaseName from treatment t
join disease d using(diseaseID)
join person p on t.patientID = p.personID
join address a on a.addressID = p.addressID),
cte2 as (
select round(count(c1.personID)/2) as patient_count , c1.address1, c1.state, c1.city, c1.diseaseName
from cte c1
join cte c2 on c1.addressID = c2.addressID
where c1.address1 = c2.address1 and c1.state = c2.state and c1.city = c2.city and c1.diseaseName = c2.diseaseName and c1.personID != c2.personID
group by c1.address1, c1.state, c1.city, c1.diseaseName
having patient_count > 1)
select diseaseName, count(patient_count) as number_of_household
from cte2
group by diseaseName
order by number_of_household desc;

-- Problem Statement 5:  An Insurance company wants a state wise report of the treatments to claim ratio between 1st April 2021
--  and 31st March 2022 (days both included). Assist them to create such a report.

select a.state ,count(c.claimID)/count(t.treatmentID) as ratio 
from treatment t 
left join claim c on t.claimID = c.claimID
join person p on t.patientID = p.personID
join address a on p.addressID = a.addressID
where t.date between '2021-04-01' and '2022-03-31'
group by a.state;