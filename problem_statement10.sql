-- Problem Statement 1:
-- The healthcare department has requested a system to analyze the performance of insurance companies and their plan.
-- For this purpose, create a stored procedure that returns the performance of different insurance plans of an insurance
--  company. When passed the insurance company ID the procedure should generate and return all the insurance plan names
--  the provided company issues, the number of treatments the plan was claimed for, and the name of the disease the plan
--  was claimed for the most. The plans which are claimed more are expected to appear above the plans that are claimed less.

delimiter $$

create procedure performanceAnalyzer(in company_id int)
begin
start transaction;
drop table if exists t1;
create temporary table t1 as
select ip.planName,
count(t.treatmentID) as total_treated
from treatment t
join claim c on c.claimID = t.claimID
join disease d on d.diseaseID = t.diseaseID
join insuranceplan ip on c.uin = ip.uin
where ip.companyID = company_id
group by ip.planName;

drop table if exists t2;
create temporary table t2 as
select planName, diseaseName, num_treated from (select ip.planName, d.diseaseName,
count(t.treatmentID) as num_treated,
dense_rank() over(partition by planName order by count(t.treatmentID) desc) as rankk
from treatment t
join claim c on c.claimID = t.claimID
join disease d on d.diseaseID = t.diseaseID
join insuranceplan ip on c.uin = ip.uin
where ip.companyID =  company_id
group by ip.planName, d.diseaseName) d
where rankk = 1;

SELECT 
    t1.planName, t1.total_treated, t2.diseaseName
FROM
    t1
        JOIN
    t2 ON t1.planName = t2.planName;

end;
end$$

delimiter ;

call performanceAnalyzer(8247);

-- Problem Statement 2:
-- It was reported by some unverified sources that some pharmacies are more popular for certain diseases. The healthcare 
-- department wants to check the validity of this report.
-- Create a stored procedure that takes a disease name as a parameter and would return the top 3 pharmacies the patients 
-- are preferring for the treatment of that disease in 2021 as well as for 2022.
-- Check if there are common pharmacies in the top 3 list for a disease, in the years 2021 and the year 2022.
-- Call the stored procedure by passing the values “Asthma” and “Psoriasis” as disease names and draw a conclusion from 
-- the result.

delimiter $$

create procedure popularityIssue (in disease_name1 varchar(150), disease_name2 varchar(150))	
begin
start transaction;

drop table if exists t1;
create temporary table t1 as 
with cte as(
select ph.pharmacyName, count(*) as count, d.diseaseName
from treatment t
join disease d on d.diseaseID = t.diseaseID
join prescription pr on pr.treatmentID = t.treatmentID
join pharmacy ph on ph.pharmacyID = pr.pharmacyID
where d.diseaseName = disease_name1 and year(t.date) in (2021,2022)
group by ph.pharmacyName, d.diseaseName)
select pharmacyName, count, diseaseName, rn from(select pharmacyName, count, diseaseName,
dense_rank() over (order by count desc) as rn
from cte) d
where rn < 4;

drop table if exists t2;
create temporary table t2 as 
with cte as(
select ph.pharmacyName, count(*) as count, d.diseaseName
from treatment t
join disease d on d.diseaseID = t.diseaseID
join prescription pr on pr.treatmentID = t.treatmentID
join pharmacy ph on ph.pharmacyID = pr.pharmacyID
where d.diseaseName = disease_name2 and year(t.date) in (2021,2022)
group by ph.pharmacyName, d.diseaseName)
select pharmacyName, count, diseaseName, rn from(select pharmacyName, count, diseaseName,
dense_rank() over (order by count desc) as rn
from cte) d
where rn < 4;

select t1.pharmacyName , t1.rn as rankk from t1,t2 where t1.pharmacyName = t2.pharmacyName and t1.rn = t2.rn and t1.diseaseName != t2.diseaseName;

end;
end$$

delimiter ;

call popularityIssue('asthma','Psoriasis');
call popularityIssue('cancer','Psoriasis');
call popularityIssue('Psoriasis', 'Depression');

-- Problem Statement 3:
-- Jacob, as a business strategist, wants to figure out if a state is appropriate for setting up an insurance company 
-- or not.
-- Write a stored procedure that finds the num_patients, num_insurance_companies, and insurance_patient_ratio, the stored 
-- procedure should also find the avg_insurance_patient_ratio and if the insurance_patient_ratio of the given state is less 
-- than the avg_insurance_patient_ratio then it Recommendation section can have the value “Recommended” otherwise the value 
-- can be “Not Recommended”.
delimiter $$
create procedure stateRecommendation (in state varchar(100), out recommendation varchar(150))
begin
declare num_patients, num_insurance_companies, insurance_patient_ratio, avg_insurance_patient_ratio int default 0;
declare exit handler for 1365
begin
set recommendation = "Recommended as the given state does not have any insurance company";
end;
start transaction;
SELECT COUNT(distinct p1.patientID) INTO num_patients FROM patient p1
JOIN person p2 ON p1.patientID = p2.personID
JOIN address a ON a.addressID = p2.addressID
WHERE a.state = state;

SELECT COUNT(distinct ic.companyID) INTO num_insurance_companies FROM insurancecompany ic
JOIN address a ON ic.addressID = a.addressID
WHERE a.state = state;

SELECT num_patients / num_insurance_companies INTO insurance_patient_ratio;

SELECT (SELECT COUNT(*)FROM patient) / (SELECT COUNT(*) FROM insurancecompany)
INTO avg_insurance_patient_ratio;

if insurance_patient_ratio < avg_insurance_patient_ratio
then set recommendation = 'Recommended';
else set recommendation = 'Not Recommended';
end if;
end;
end$$
delimiter ;
call stateRecommendation('MA', @recommendation);
select @recommendation;
call stateRecommendation('TN', @recommendation);
select @recommendation;
call stateRecommendation('AA', @recommendation);
select @recommendation;


-- Problem Statement 4:
-- Currently, the data from every state is not in the database, The management has decided to add the data from other 
-- states and cities as well. It is felt by the management that it would be helpful if the date and time were to be stored 
-- whenever new city or state data is inserted.
-- The management has sent a requirement to create a PlacesAdded table if it doesn’t already exist, that has four attributes. 
-- placeID, placeName, placeType, and timeAdded.
-- Description
-- placeID: This is the primary key, it should be auto-incremented starting from 1
-- placeName: This is the name of the place which is added for the first time
-- placeType: This is the type of place that is added for the first time. The value can either be ‘city’ or ‘state’
-- timeAdded: This is the date and time when the new place is added
-- You have been given the responsibility to create a system that satisfies the requirements of the management. Whenever some 
-- data is inserted in the Address table that has a new city or state name, the PlacesAdded table should be updated with relevant data. 

create table if not exists PlacesAdded(
placeID int auto_increment primary key,
PlaceName varchar(100) unique,
placeType enum('state','city'),
timeAdded datetime default current_timestamp()
);

delimiter $$
drop trigger if exists addPlace;
create trigger addPlace 
after insert on address for each row
begin
if exists(
	select 1 from address where state = new.state
) and not exists(
	select 1 from placesadded where placetype = 'state' and placename = new.state
)then insert into placesadded(placename, placetype) values(new.state, 'state');
end if;
if exists(
	select 1 from address where city = new.city
) and not exists(
	select 1 from placesadded where placetype = 'city' and placename = new.city
)then insert into placesadded(placename, placetype) values(new.city, 'city');
end if;
end $$

delimiter ;
insert into address values(1000000, 'bibwewadi', 'Pune', 'MH', 411037);
insert into address values(1000002, 'kothrud', 'Pune', 'MH', 411041);
insert into address values(1000003, 'anandnagar', 'jamner', 'MH', 424241);
insert into address values(1000004, 'Kishorsangha', 'Alipurduar', 'WB', 736121);

-- Problem Statement 5:
-- Some pharmacies suspect there is some discrepancy in their inventory management. The quantity in the ‘Keep’ is updated
--  regularly and there is no record of it. They have requested to create a system that keeps track of all the transactions 
--  whenever the quantity of the inventory is updated.
-- You have been given the responsibility to create a system that automatically updates a Keep_Log table which has  the following fields:
-- id: It is a unique field that starts with 1 and increments by 1 for each new entry
-- medicineID: It is the medicineID of the medicine for which the quantity is updated.
-- quantity: The quantity of medicine which is to be added. If the quantity is reduced then the number can be negative.
-- For example:  If in Keep the old quantity was 700 and the new quantity to be updated is 1000, then in Keep_Log the quantity should be 300.
-- Example 2: If in Keep the old quantity was 700 and the new quantity to be updated is 100, then in Keep_Log the quantity should be -600.
         
create table if not exists keepLog(
id int auto_increment primary key,
medicineID int references medicine(medicineID),
quantity int);

delimiter $$

drop trigger if exists keepUpdates $$
create trigger keepUpdates 
after update on keep 
for each row
begin
declare changes int;
set changes = new.quantity - old.quantity;
insert into keepLog (medicineID, quantity) values (new.medicineID, changes);
end $$

delimiter ;

update keep
set quantity = 1000 
where pharmacyID = 5527 and medicineID = 1;

select * from keeplog;