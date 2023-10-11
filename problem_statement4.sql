-- Problem Statement 1: 
-- “HealthDirect” pharmacy finds it difficult to deal with the product type of medicine being displayed in numerical form, they
--  want the product type in words. Also, they want to filter the medicines based on tax criteria. 
-- Display only the medicines of product categories 1, 2, and 3 for medicines that come under tax category I and medicines of product 
-- categories 4, 5, and 6 for medicines that come under tax category II.
-- Write a SQL query to solve this problem.
-- ProductType numerical form and ProductType in words are given by
-- 1 - Generic, 
-- 2 - Patent, 
-- 3 - Reference, 
-- 4 - Similar, 
-- 5 - New, 
-- 6 - Specific,
-- 7 - Biological, 
-- 8 – Dinamized
-- 3 random rows and the column names of the Medicine table are given for reference.
-- Medicine (medicineID, companyName, productName, description, substanceName, productType, taxCriteria, hospitalExclusive,
--  governmentDiscount, taxImunity, maxPrice
-- 12	LIBRA COMERCIO DE PRODUTOS FARMACEUTICOS LTDA	OXALIPLATINA	100 MG PO LIOFILIZADO FR/AMP X 1000 MG
-- 	NC/NI	1	I	N	N	N	2373.63
-- 13	LIBRA COMERCIO DE PRODUTOS FARMACEUTICOS LTDA	SULBACTAM SODICO + AMPICILINA SODICA	1 G + 2 G CT FR
--  AMP VD INC	NC/NI	4	II	N	N	N	29.59
-- 14	LIBRA COMERCIO DE PRODUTOS FARMACEUTICOS LTDA	PACLITAXEL	6 MG/ML SOL INJ CT FR/AMP X 50 ML	NC/NI	
-- 1	I	N	N	N	4122.12
SELECT 
    *
FROM
    (SELECT 
        medicineID,
            CASE
                WHEN
                    taxCriteria = 'I'
                THEN
                    CASE
                        WHEN productType = 1 THEN 'Generic'
                        WHEN productType = 2 THEN 'Patent'
                        WHEN productType = 3 THEN 'Reference'
                    END
                WHEN
                    taxCriteria = 'II'
                THEN
                    CASE
                        WHEN productType = 4 THEN 'Similar'
                        WHEN productType = 5 THEN 'New'
                        WHEN productType = 6 THEN 'Specific'
                    END
            END AS product_type
    FROM
        healthcare.medicine) d
WHERE
    d.product_type IS NOT NULL;

-- Problem Statement 2:  
-- 'Ally Scripts' pharmacy company wants to find out the quantity of medicine prescribed in each of its prescriptions.
-- Write a query that finds the sum of the quantity of all the medicines in a prescription and if the total quantity
--  of medicine is less than 20 tag it as “low quantity”. If the quantity of medicine is from 20 to 49 (both numbers
--  including) tag it as “medium quantity“ and if the quantity is more than equal to 50 then tag it as “high quantity”.
-- Show the prescription Id, the Total Quantity of all the medicines in that prescription, and the Quantity tag for
--  all the prescriptions issued by 'Ally Scripts'.
-- 3 rows from the resultant table may be as follows:
-- prescriptionID	totalQuantity	Tag
-- 1147561399		43			Medium Quantity
-- 1222719376		71			High Quantity
-- 1408276190		48			Medium Quantity

SELECT 
    ph.pharmacyName,
    pr.prescriptionID,
    SUM(quantity) AS quantity,
    CASE
        WHEN SUM(quantity) < 20 THEN 'low quantity'
        WHEN SUM(quantity) BETWEEN 20 AND 49 THEN 'Medium Quantity'
        WHEN SUM(quantity) >= 50 THEN 'High Quantity'
    END AS quantity_category
FROM
    prescription pr
        JOIN
    pharmacy ph USING (pharmacyID)
        JOIN
    contain c USING (prescriptionID)
GROUP BY ph.pharmacyName , pr.prescriptionID
HAVING ph.pharmacyName = 'Ally Scripts';

-- Problem Statement 3: 
-- In the Inventory of a pharmacy 'Spot Rx' the quantity of medicine is considered ‘HIGH QUANTITY’ when the quantity exceeds
-- 7500 and ‘LOW QUANTITY’ when the quantity falls short of 1000. The discount is considered “HIGH” if the discount rate on
-- a product is 30% or higher, and the discount is considered “NONE” when the discount rate on a product is 0%.
-- 'Spot Rx' needs to find all the Low quantity products with high discounts and all the high-quantity products with no 
-- discount so they can adjust the discount rate according to the demand. 
-- Write a query for the pharmacy listing all the necessary details relevant to the given requirement.
-- Hint: Inventory is reflected in the Keep table.
SELECT
       ph.pharmacyName,
       m.medicineID,
       k.quantity,
    CASE
        WHEN quantity < 1000 THEN 'low quantity'
        WHEN quantity >= 7500 THEN 'High Quantity'
        ELSE 'Medium Quantity'
    END AS quantity_category,
    k.discount,
    case
    when discount = 0 then "None"
    when discount >= 30 then "High"
    else "Normal"
    end as discount_category
FROM
    pharmacy ph
        JOIN
    keep k USING (pharmacyID)
        JOIN
    medicine m USING (medicineID)
where ph.pharmacyName = 'Spot Rx' and ((quantity < 1000 and discount >= 30) or (quantity >= 7500 and discount = 0));

-- Problem Statement 4: 
-- Mack, From HealthDirect Pharmacy, wants to get a list of all the affordable and costly, hospital-exclusive medicines
-- in the database. Where affordable medicines are the medicines that have a maximum price of less than 50% of the avg 
-- maximum price of all the medicines in the database, and costly medicines are the medicines that have a maximum price
-- of more than double the avg maximum price of all the medicines in the database.  Mack wants clear text next to each
-- medicine name to be displayed that identifies the medicine as affordable or costly. The medicines that do not fall 
-- under either of the two categories need not be displayed.
-- Write a SQL query for Mack for this requirement.
SELECT 
    *
FROM
    (SELECT 
        m.medicineID,
            m.maxPrice,
            CASE
                WHEN
                    m.maxPrice < 0.5 * (SELECT 
                            AVG(maxPrice)
                        FROM
                            medicine)
                THEN
                    'Affordable'
                WHEN
                    m.maxPrice > 2 * (SELECT 
                            AVG(maxPrice)
                        FROM
                            medicine)
                THEN
                    'Costly'
            END AS cost
    FROM
        keep k
    JOIN pharmacy p ON k.pharmacyID = p.pharmacyID
    JOIN medicine m ON k.medicineID = m.medicineID
    WHERE
        p.pharmacyName = 'HealthDirect'
            AND m.hospitalExclusive = 'S') d
WHERE
    cost IS NOT NULL;

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
SELECT 
    p2.personName,
    p2.gender,
    p.dob,
    CASE
        WHEN
            p.dob >= '2005-01-01' AND p2.gender = 'male'
        THEN
            'Young Male'
        WHEN
            p.dob >= '2005-01-01' AND p2.gender = 'female'
        THEN
            'Young Female'
        WHEN
            p.dob < '2005-01-01' AND p.dob >= '1985-01-01' AND p2.gender = 'male'
        THEN
            'Adult Male'
        WHEN
            p.dob < '2005-01-01' AND p.dob >= '1985-01-01' AND p2.gender = 'female'
        THEN
            'Adult Female'
        WHEN
            p.dob < '1985-01-01' AND p.dob >= '1970-01-01' AND p2.gender = 'male'
        THEN
            'MidAge Male'
        WHEN
            p.dob < '1985-01-01' AND p.dob >= '1970-01-01' AND p2.gender = 'female'
        THEN
            'MidAge Female'
        WHEN
            p.dob < '1970-01-01' AND p2.gender = 'male'
        THEN
            'Elder Male'
        WHEN
            p.dob < '1970-01-01' AND p2.gender = 'female'
        THEN
            'Elder Female'
    END AS Category
FROM
    patient p
        JOIN
    person p2 ON p2.personID = p.patientID;