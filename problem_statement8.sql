-- Query 1: 
SELECT 
    TIMESTAMPDIFF(YEAR, dob, t.date) AS age,
    COUNT(*) AS numTreatments
FROM
    Patient p
        JOIN
    Treatment t USING (patientID)
GROUP BY age
ORDER BY age;
-- grouping by TIMESTAMPDIFF(YEAR, dob, t.date) rather than DATEDIFF(hour, dob , GETDATE())/8766
-- join with using rather than on 
-- order by age rather than count

-- Query 2: 
SELECT 
    city,
    COUNT(DISTINCT pharmacyID) AS total_pharmacy,
    COUNT(DISTINCT companyID) AS total_insurance_company,
    COUNT(DISTINCT personID) AS total_people
FROM
    address
        LEFT JOIN
    pharmacy USING (addressID)
        LEFT JOIN
    insurancecompany USING (addressID)
        LEFT JOIN
    person USING (addressID)
GROUP BY city
ORDER BY total_people DESC;
-- using left join and reducing the number of necessary joins and need for creating temporary tables
-- single query is better than multiple queries
-- creating temporary tables takes more space

-- Query 3: 
SELECT 
    c.prescriptionID,
    SUM(quantity) AS total_quantity,
    CASE
        WHEN SUM(quantity) < 20 THEN 'Low Quantity'
        WHEN SUM(quantity) < 50 THEN 'Medium Quantity'
        ELSE 'High Quantity'
    END AS tag
FROM
    contain c
        JOIN
    prescription p USING (prescriptionid)
        JOIN
    pharmacy ph USING (pharmacyid)
WHERE
    pharmacyname = 'Ally Scripts'
GROUP BY c.prescriptionid;
-- using is better join condition as it uses equi join

-- Query 4: 
with avg_quantity_cte as (
select sum(quantity) as totalquantity from pharmacy p 
join prescription pr using (pharmacyid)
join treatment t using (treatmentid)
join contain c using (prescriptionid)
group by p.pharmacyID,pr.prescriptionID
)
select p.pharmacyid,pr.prescriptionid,sum(quantity) as totalquantity 
from Pharmacy p
join Prescription pr using (pharmacyid)
join Contain using (prescriptionid)
join Medicine ON Medicine.medicineID = Contain.medicineID
join Treatment t using (treatmentid)
where year(date) = 2022
group by p.pharmacyID, pr.prescriptionID
having totalQuantity > (select avg(totalquantity) from avg_quantity_cte);
-- when using in single query cte is better than temporary table

-- Query 5: 
SELECT 
    diseasename, COUNT(*) AS total_claims
FROM
    disease
        JOIN
    treatment USING (diseaseid)
        JOIN
    claim USING (claimid)
WHERE
    diseaseName LIKE '%p%'
GROUP BY diseaseName;
-- direct filtering is better than subquery