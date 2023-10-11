-- Problem Statement 1:
-- Insurance companies want to know if a disease is claimed higher or lower than average.  Write a stored procedure that returns 
-- “claimed higher than average” or “claimed lower than average” when the diseaseID is passed to it. 
-- Hint: Find average number of insurance claims for all the diseases.  If the number of claims for the passed disease is higher 
-- than the average return “claimed higher than average” otherwise “claimed lower than average”.
delimiter $$

create procedure rate_of_claim (in diseaseId int, out rate varchar(150))
begin
declare avg_claim, claimed int default 0;

start transaction;
SELECT 
    COUNT(DISTINCT c.claimID) / COUNT(DISTINCT d.diseaseID)
INTO avg_claim FROM
    treatment t
        JOIN
    claim c ON t.claimID = c.claimID
        JOIN
    disease d ON t.diseaseID = d.diseaseID;

SELECT 
    COUNT(d.diseaseID)
INTO claimed FROM
    treatment t
        JOIN
    claim c ON t.claimID = c.claimID
        JOIN
    disease d ON t.diseaseID = d.diseaseID
WHERE
    d.diseaseID = diseaseID;

if claimed > avg_claim then 
set rate = 'claimed higher than average';
else set rate = 'claimed lower than average';
end if;

end$$

delimiter ;

call rate_of_claim(1, @rate);
select @rate;

-- Problem Statement 2:  
-- Joseph from Healthcare department has requested for an application which helps him get genderwise report for any disease. 
-- Write a stored procedure when passed a disease_id returns 4 columns,
-- disease_name, number_of_male_treated, number_of_female_treated, more_treated_gender
-- Where, more_treated_gender is either ‘male’ or ‘female’ based on which gender underwent more often for the disease, if the number
--  is same for both the genders, the value should be ‘same’.
delimiter $$

create procedure genderwise_report (in diseaseID int)
begin
start transaction;

SELECT 
    d.diseaseName,
    SUM(IF(p.gender = 'male', 1, 0)) AS number_of_male_treated,
    SUM(IF(p.gender = 'female', 1, 0)) AS number_of_male_treated,
    CASE
        WHEN SUM(IF(p.gender = 'male', 1, 0)) < SUM(IF(p.gender = 'female', 1, 0)) THEN 'female'
        ELSE 'male'
    END AS most_treated_gender
FROM
    disease d
        JOIN
    treatment t ON t.diseaseID = d.diseaseID
        JOIN
    person p ON p.personID = t.patientID
WHERE
    d.diseaseID = diseaseID
GROUP BY d.diseaseName;

end;
end$$
delimiter ;

call genderwise_report(12);	


-- Problem Statement 3:  
-- The insurance companies want a report on the claims of different insurance plans. 
-- Write a query that finds the top 3 most and top 3 least claimed insurance plans.
-- The query is expected to return the insurance plan name, the insurance company name which has that plan, and whether the plan 
-- is the most claimed or least claimed. 
with cte as(
	select
		planName,
		count(*) as claim_count	,
		dense_rank() over(order by count(*) desc) as high,
		dense_rank() over(order by count(*)) as low
	from
		claim c 
	join insuranceplan i using (uin)
	group by i.planName
)
select 
	ic.companyName,
	cte.planName,
	case 
		when cte.high <= 3 then 'Most Claimed'
		when cte.low <= 3 then 'Least Claimed'
	end as ClaimStatus
from 
	cte
join insuranceplan i on cte.planName = i.planName 
join insuranceCompany ic on i.companyID = ic.companyID
where cte.high <= 3 or cte.low <= 3;

-- Problem Statement 4: 
-- The healthcare department wants to know which category of patients is being affected the most by each disease.
-- Assist the department in creating a report regarding this.
-- Provided the healthcare department has categorized the patients into the following category.
-- YoungMale: Born on or after 1st Jan  2005  and gender male.
-- YoungFemale: Born on or after 1st Jan  2005  and gender female.
-- AdultMale: Born before 1st Jan 2005 but on or after 1st Jan 1985 and gender male.
-- AdultFemale: Born before 1st Jan 2005 but on or after 1st Jan 1985 and gender female.
-- MidAgeMale: Born before 1st Jan 1985 but on or after 1st Jan 1970 and gender male.
-- MidAgeFemale: Born before 1st Jan 1985 but on or after 1st Jan 1970 and gender female.
-- ElderMale: Born before 1st Jan 1970, and gender male.
-- ElderFemale: Born before 1st Jan 1970, and gender female.
with cte as(
SELECT 
    p2.personID,
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
            p.
            dob < '1970-01-01' AND p2.gender = 'female'
        THEN
            'Elder Female'
    END AS Category
FROM
    patient p
        JOIN
    person p2 ON p2.personID = p.patientID),
cte2 as(
SELECT 
    c.personID, c.Category, d.diseaseName
FROM
    disease d
        JOIN
    treatment t ON t.diseaseID = d.diseaseID
        JOIN
    cte c ON t.patientID = c.personID),
cte3 as(
SELECT 
    diseaseName, category, COUNT(personID) as num
FROM
    cte2
GROUP BY diseaseName , category)
select c.diseaseName, c.category from cte3 c
where c.category = (select max(num) from cte3 where diseaseName = c.diseaseName);

-- Problem Statement 5:  
-- Anna wants a report on the pricing of the medicine. She wants a list of the most expensive and most affordable medicines only. 
-- Assist anna by creating a report of all the medicines which are pricey and affordable, listing the companyName, productName, 
-- description, maxPrice, and the price category of each. Sort the list in descending order of the maxPrice.
-- Note: A medicine is considered to be “pricey” if the max price exceeds 1000 and “affordable” if the price is under 5. Write a
--  query to find 
select 
	m.companyName,
	m.productName,
	m.description,
	m.maxPrice,
	case 
		when m.maxPrice > 1000 then 'pricey'
		when m.maxPrice < 5 then 'affordable'
	end as PriceCategory
from 
	medicine m 
where m.maxPrice > 1000 or m.maxPrice < 5
order by m.maxPrice desc;