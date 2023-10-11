-- Problem Statement 1: 
-- Johansson is trying to prepare a report on patients who have gone through treatments more than once. Help Johansson 
-- prepare a report that shows the patient's name, the number of treatments they have undergone, and their age, Sort the
-- data in a way that the patients who have undergone more treatments appear on top.
SELECT 
    p1.personName,
    p2.total_treatment,
    TIMESTAMPDIFF(YEAR, p2.dob, CURDATE()) AS age
FROM
    person p1
        JOIN
    (SELECT 
        p.patientID,
            COUNT(treatmentID) AS total_treatment,
            (SELECT 
                    dob
                FROM
                    patient
                WHERE
                    patientID = p.patientID) AS dob
    FROM
        patient p
    JOIN treatment t ON t.patientID = p.patientID
    GROUP BY p.patientID) p2 ON p2.patientId = p1.personID
ORDER BY p2.total_treatment DESC;

-- Problem Statement 2:  
-- Bharat is researching the impact of gender on different diseases, He wants to analyze if a certain disease is more likely 
-- to infect a certain gender or not.
-- Help Bharat analyze this by creating a report showing for every disease how many males and females underwent treatment for 
-- each in the year 2021. It would also be helpful for Bharat if the male-to-female ratio is also shown.

SELECT 
    d.diseaseName,
    COUNT(p1.gender) AS total_count,
    SUM(p1.male) AS total_male,
    SUM(p1.female) AS total_female,
    SUM(p1.female) / SUM(p1.male) AS ratio
FROM
    treatment t
        JOIN
    disease d ON d.diseaseID = t.diseaseID
        JOIN
    person p ON p.personID = t.patientID
        JOIN
    (SELECT 
        personID,
            gender,
            IF(gender = 'male', 1, 0) AS male,
            IF(gender = 'female', 1, 0) AS female
    FROM
        person) p1 ON p1.personID = p.personID
WHERE
    YEAR(t.date) = '2021'
GROUP BY d.diseaseName;

-- Problem Statement 3:  
-- Kelly, from the Fortis Hospital management, has requested a report that shows for each disease, the top 3 cities that had 
-- the most number treatment for that disease.
-- Generate a report for Kelly’s requirement.
select * from (
select a.city, d.diseaseName, count(t.patientID) as count_p, 
dense_rank() over (partition by a.city order by count(t.patientID) desc) as rankk
from treatment t
join disease d on d.diseaseID = t.diseaseID
join person p on p.personID = t.patientID
join address a on a.addressID = p.addressID
group by a.city, d.diseaseName) d
where d.rankk in (1,2,3);

-- Problem Statement 4: 
-- Brooke is trying to figure out if patients with a particular disease are preferring some pharmacies over others or not, 
-- For this purpose, she has requested a detailed pharmacy report that shows each pharmacy name, and how many prescriptions 
-- they have prescribed for each disease in 2021 and 2022, She expects the number of prescriptions prescribed in 2021 and 2022 
-- be displayed in two separate columns.
-- Write a query for Brooke’s requirement.
SELECT 
    diseaseName,
    pharmacyName,
    SUM(IF(YEAR(t.date) = 2021, 1, 0)) AS in_2021,
    SUM(IF(YEAR(t.date) = 2022, 1, 0)) AS in_2022
FROM
    treatment t
        JOIN
    disease d ON d.diseaseID = t.diseaseID
        JOIN
    prescription pr ON t.treatmentID = pr.treatmentID
        JOIN
    pharmacy ph ON pr.pharmacyID = ph.pharmacyID
GROUP BY pharmacyName , diseaseName
ORDER BY pharmacyName; 

-- Problem Statement 5:  
-- Walde, from Rock tower insurance, has sent a requirement for a report that presents which insurance company is targeting 
-- the patients of which state the most. 
-- Write a query for Walde that fulfills the requirement of Walde.
-- Note: We can assume that the insurance company is targeting a region more if the patients of that region are claiming more 
-- insurance of that company.
select state, companyName, total from (select a.state, ic.companyName, count(personID) as total,
dense_rank() over(partition by state order by count(personID) desc) as rankk
from insurancecompany ic
join insuranceplan ip on ip.companyID = ic.companyID
join address a on a.addressID = ic.addressID
join person p on p.addressID = a.addressID
group by ic.companyName, a.state
order by a.state, total desc) d
where rankk = 1
order by total desc;


