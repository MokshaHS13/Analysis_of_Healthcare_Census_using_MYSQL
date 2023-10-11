-- Problem Statement 1
-- Jimmy, from the healthcare department, has requested a report that shows how the number of treatments each age category of patients
--  has gone through in the year 2022. 
-- The age category is as follows, Children (00-14 years), Youth (15-24 years), Adults (25-64 years), and Seniors (65 years and over).
-- Assist Jimmy in generating the report

-- using where
SELECT 
    CASE
        WHEN (YEAR(t.date) - YEAR(p.dob)) <= 14 THEN 'child'
        WHEN
            (YEAR(t.date) - YEAR(p.dob)) >= 15
                AND (YEAR(t.date) - YEAR(p.dob)) <= 24
        THEN
            'youth'
        WHEN
            (YEAR(t.date) - YEAR(p.dob)) >= 25
                AND (YEAR(t.date) - YEAR(p.dob)) <= 64
        THEN
            'adults'
        ELSE 'senior'
    END AS age_category,
    COUNT(t.treatmentID) AS num_of_treatments
FROM
    treatment t
        JOIN
    patient p ON p.patientID = t.patientID
        JOIN
    disease d ON d.diseaseID = t.diseaseID
WHERE
    YEAR(t.date) = '2022'
GROUP BY age_category;

-- using if
SELECT 
    IF(YEAR(t.date) - YEAR(p.dob) <= 14,
        'child',
        IF(YEAR(t.date) - YEAR(p.dob) >= 15
                AND (YEAR(t.date) - YEAR(p.dob)) <= 24,
            'youth',
            IF(YEAR(t.date) - YEAR(p.dob) >= 25
                    AND (YEAR(t.date) - YEAR(p.dob)) <= 64,
                'adults',
                'senior'))) AS age_category,
    COUNT(t.treatmentID) AS num_of_treatments
FROM
    treatment t
        JOIN
    patient p ON p.patientID = t.patientID
        JOIN
    disease d ON d.diseaseID = t.diseaseID
WHERE
    YEAR(t.date) = '2022'
GROUP BY age_category;

-- problem statement 2
-- Jimmy, from the healthcare department, wants to know which disease is infecting people of which gender more often.
-- Assist Jimmy with this purpose by generating a report that shows for each disease the male-to-female ratio. Sort the data in a way that is helpful for Jimmy.

-- using cte
with cte as(
SELECT 
    personID, gender,
    IF(gender = 'male', 1, 0) AS male,
    IF(gender = 'female', 1, 0) AS female
FROM
    person)
SELECT 
    diseaseName,
    SUM(male) / COUNT(gender) AS ratio_male,
    SUM(female) / COUNT(gender) AS ratio_female
FROM
    treatment t
        JOIN
    disease d ON d.diseaseID = t.diseaseID
        JOIN
    cte c ON c.personID = t.patientID
GROUP BY diseaseName;

-- using derived table
SELECT 
    diseaseName,
    SUM(male) / COUNT(gender) AS ratio_male,
    SUM(female) / COUNT(gender) AS ratio_female
FROM
    treatment t
        JOIN
    person p ON p.personID = t.patientID
        JOIN
    disease d ON d.diseaseID = t.diseaseID
        JOIN
    (SELECT 
        personID,
            IF(gender = 'male', 1, 0) AS male,
            IF(gender = 'female', 1, 0) AS female
    FROM
        person) c ON c.personID = p.personID
GROUP BY diseaseName;

-- Problem Statement 3: Jacob, from insurance management, has noticed that insurance claims are not made for all the treatments. 
-- He also wants to figure out if the gender of the patient has any impact on the insurance claim. Assist Jacob in this situation 
-- by generating a report that finds for each gender the number of treatments, number of claims, and treatment-to-claim ratio. 
-- And notice if there is a significant difference between the treatment-to-claim ratio of male and female patients.

SELECT 
    p.gender,
    COUNT(t.treatmentID) AS treatments,
    COUNT(c.claimID) AS claims,
    COUNT(c.claimID) / COUNT(t.treatmentID) AS ratio
FROM
    treatment t
        LEFT JOIN
    person p ON p.personID = t.patientID
        LEFT JOIN
    claim c ON c.claimID = t.claimID
GROUP BY p.gender;

-- Problem Statement 4: The Healthcare department wants a report about the inventory of pharmacies. Generate a report 
-- on their behalf that shows how many units of medicine each pharmacy has in their inventory, the total maximum retail 
-- price of those medicines, and the total price of all the medicines after discount. 
-- Note: discount field in keep signifies the percentage of discount on the maximum price.

-- using derived table
SELECT 
    c.pharmacyName,
    SUM(c.quantity) AS total_units,
    ROUND(SUM(c.maxPrice), 2) AS total_price,
    ROUND(SUM(c.price), 2) AS total_discounted_price
FROM
    medicine m
        JOIN
    (SELECT 
        p.pharmacyName,
            m.productName,
            k.quantity,
            m.maxPrice,
            m.medicineId,
            m.maxPrice - (m.maxPrice / k.discount) AS price
    FROM
        medicine m
    JOIN keep k ON k.medicineID = m.medicineID
    JOIN pharmacy p ON k.pharmacyID = p.pharmacyID) c USING (medicineID)
GROUP BY c.pharmacyName;

-- using cte
with cte as(
SELECT 
    p.pharmacyName,
    m.productName,
    k.quantity,
    m.maxPrice*k.quantity as maxPrice,
    m.medicineId,
    m.maxPrice*k.quantity - (m.maxPrice*k.quantity / k.discount) AS price
FROM
    medicine m
        JOIN
    keep k ON k.medicineID = m.medicineID
        JOIN
    pharmacy p ON k.pharmacyID = p.pharmacyID)
SELECT 
    c.pharmacyName,
    SUM(c.quantity) AS total_units,
    ROUND(SUM(c.maxPrice), 2) AS total_price,
    ROUND(SUM(c.price), 2) AS total_discounted_price
FROM
    medicine m
        JOIN cte c USING (medicineID)
GROUP BY c.pharmacyName;

-- Problem Statement 5:  The healthcare department suspects that some pharmacies prescribe more medicines than others 
-- in a single prescription, for them, generate a report that finds for each pharmacy the maximum, minimum and average 
-- number of medicines prescribed in their prescriptions. 

SELECT 
    p.prescriptionID,
    SUM(c.quantity) AS total,
    MAX(c.quantity) AS max_quantity,
    MIN(c.quantity) AS min_quantity,
    AVG(quantity) AS avg_quantity
FROM
    contain c
        JOIN
    medicine m USING (medicineID)
        JOIN
    prescription p USING (prescriptionID)
GROUP BY p.prescriptionID;
