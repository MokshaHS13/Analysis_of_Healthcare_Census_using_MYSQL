-- Problem Statement 1:
-- Patients are complaining that it is often difficult to find some medicines. They move from pharmacy to pharmacy to get the required 
-- medicine. A system is required that finds the pharmacies and their contact number that have the required medicine in their inventory. 
-- So that the patients can contact the pharmacy and order the required medicine.
-- Create a stored procedure that can fix the issue.
delimiter $$

create procedure findPharmacy (in company varchar(100), product varchar(100))
begin
DECLARE EXIT HANDLER FOR SQLEXCEPTION
BEGIN
SELECT 'An error has occurred, operation rollbacked and the stored procedure was terminated';
END;
start transaction;
SELECT p.pharmacyName, p.phone
FROM medicine m 
JOIN keep k USING (medicineID)
JOIN pharmacy p USING (pharmacyID)
WHERE m.productName = product AND m.companyname = company;
end;
end$$

delimiter ;

call findPharmacy('LUPER INDUSTRIA FARMACEUTICA LTDA','CETIL');

-- Problem Statement 2:
-- The pharmacies are trying to estimate the average cost of all the prescribed medicines per prescription, for all the prescriptions 
-- they have prescribed in a particular year. Create a stored function that will return the required value when the pharmacyID and year 
-- are passed to it. Test the function with multiple values.
delimiter &&
drop function if exists estimateCost;
create function estimateCost (pharmacyID int, year int)
returns int
reads sql data
begin 
declare cost int;

SELECT 
    AVG(m.maxPrice - (m.maxPrice / k.discount))
INTO cost FROM
    pharmacy p
        JOIN
    keep k USING (pharmacyID)
        JOIN
    medicine m USING (medicineID)
GROUP BY p.pharmacyID
HAVING p.pharmacyID = pharmacyID;

return cost;
end&&

delimiter ;

select estimateCost(1008,2021);
select estimateCost(1145,2021);

-- Problem Statement 3:
-- The healthcare department has requested an application that finds out the disease that was spread the most in a state for a given year. 
-- So that they can use the information to compare the historical data and gain some insight.
-- Create a stored function that returns the name of the disease for which the patients from a particular state had the most number of 
-- treatments for a particular year. Provided the name of the state and year is passed to the stored function.
delimiter &&

drop function if exists findDiseases;
create function findDiseases (dstate varchar(100), dyear int)
returns varchar(100)
deterministic
begin 

declare mostSpreaded varchar(100);

select diseaseName into mostSpreaded from
(select d.diseaseName, a.state, year(t.date) as year, count(p.personID) as total, 
dense_rank() over (order by count(p.personID) desc) as dr
from treatment t
join disease d using(diseaseID)
join person p on t.patientID = p.personID
join address a using(addressID)
group by d.diseaseName, a.state, year
having a.state = dstate and year = dyear) d
where dr = 1;

return mostSpreaded;
end&&

delimiter ;

select findDiseases('AL', 2021);
select findDiseases('OK', 2020);

-- Problem Statement 4:
-- The representative of the pharma union, Aubrey, has requested a system that she can use to find how many people in a specific city 
-- have been treated for a specific disease in a specific year.
-- Create a stored function for this purpose.
delimiter &&

create function getPeopleCount (pdiseasename varchar(100), pcity varchar(100), pyear int)
returns int
deterministic
begin 
declare peopleCount int;

SELECT 
    COUNT(p.personID)
INTO peopleCount FROM
    treatment t
        JOIN
    disease d USING (diseaseID)
        JOIN
    person p ON t.patientID = p.personID
        JOIN
    address a USING (addressID)
WHERE
    d.diseaseName = pdiseasename
        AND a.city = pcity
        AND YEAR(t.date) = pyear;

return peopleCount;
end&&

delimiter ;

select getPeopleCount('cancer', 'Oklahoma City', 2021);
select getPeopleCount('Asthma', 'Washington', 2020);

-- Problem Statement 5:
-- The representative of the pharma union, Aubrey, is trying to audit different aspects of the pharmacies. She has requested a system 
-- that can be used to find the average balance for claims submitted by a specific insurance company in the year 2022. 
-- Create a stored function that can be used in the requested application. 
delimiter &&

create function claimBalance (insurance_company varchar(100))
returns int
reads sql data
begin 
declare avg_balance int;
SELECT 
    AVG(c.balance)
INTO avg_balance FROM
    treatment t
        JOIN
    claim c USING (claimID)
        JOIN
    insuranceplan ip USING (uin)
        JOIN
    insurancecompany ic USING (companyID)
WHERE
    companyName LIKE CONCAT('%', insurance_company, '%')
        AND YEAR(t.date) = 2022;
return avg_balance;
end&&

delimiter ;

select claimBalance('Bajaj Allianz General Insurance Co. Ltd.�');
select claimBalance('Future Generali India Insurance Company Limited.���');
