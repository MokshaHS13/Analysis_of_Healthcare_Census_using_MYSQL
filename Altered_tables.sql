ALTER TABLE `healthcare_1`.`claim` 
ADD INDEX `uin_idx` (`uin` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`claim` 
ADD CONSTRAINT `uin`
  FOREIGN KEY (`uin`)
  REFERENCES `healthcare_1`.`insuranceplan` (`uin`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
  
  ALTER TABLE `healthcare_1`.`contain` 
DROP PRIMARY KEY,
ADD PRIMARY KEY (`prescriptionID`, `medicineID`, `quantity`),
ADD INDEX `medicineID_idx` (`medicineID` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`contain` 
ADD CONSTRAINT `prescriptionID`
  FOREIGN KEY (`prescriptionID`)
  REFERENCES `healthcare_1`.`prescription` (`prescriptionID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
ADD CONSTRAINT `medicineID`
  FOREIGN KEY (`medicineID`)
  REFERENCES `healthcare_1`.`medicine` (`medicineID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
  
  ALTER TABLE `healthcare_1`.`insurancecompany` 
ADD INDEX `addressID_idx` (`addressID` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`insurancecompany` 
ADD CONSTRAINT `addressID`
  FOREIGN KEY (`addressID`)
  REFERENCES `healthcare_1`.`address` (`addressID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

ALTER TABLE `healthcare_1`.`insuranceplan` 
ADD INDEX `companyID_idx` (`companyID` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`insuranceplan` 
ADD CONSTRAINT `companyID`
  FOREIGN KEY (`companyID`)
  REFERENCES `healthcare_1`.`insurancecompany` (`companyID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

ALTER TABLE `healthcare_1`.`keep` 
CHANGE COLUMN `quantity` `quantity` INT NOT NULL ,
CHANGE COLUMN `discount` `discount` INT NOT NULL ,
DROP PRIMARY KEY,
ADD PRIMARY KEY (`pharmacyID`, `medicineID`, `quantity`, `discount`),
ADD INDEX `medicineID_idx` (`medicineID` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`keep` 
ADD CONSTRAINT `pharmacyID`
  FOREIGN KEY (`pharmacyID`)
  REFERENCES `healthcare_1`.`pharmacy` (`pharmacyID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
ADD CONSTRAINT `medicineID`
  FOREIGN KEY (`medicineID`)
  REFERENCES `healthcare_1`.`medicine` (`medicineID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

ALTER TABLE `healthcare_1`.`patient` 
ADD CONSTRAINT `patientID`
  FOREIGN KEY (`patientID`)
  REFERENCES `healthcare_1`.`patient` (`patientID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
  
  ALTER TABLE `healthcare_1`.`person` 
ADD INDEX `addressID_idx` (`addressID` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`person` 
ADD CONSTRAINT `addressID`
  FOREIGN KEY (`addressID`)
  REFERENCES `healthcare_1`.`address` (`addressID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

ALTER TABLE `healthcare_1`.`pharmacy` 
ADD INDEX `addressID_idx` (`addressID` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`pharmacy` 
ADD CONSTRAINT `addressID`
  FOREIGN KEY (`addressID`)
  REFERENCES `healthcare_1`.`address` (`addressID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
  
  ALTER TABLE `healthcare_1`.`prescription` 
ADD INDEX `pharmacyID_idx` (`pharmacyID` ASC) VISIBLE,
ADD INDEX `treatmentID_idx` (`treatmentID` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`prescription` 
ADD CONSTRAINT `pharmacyID`
  FOREIGN KEY (`pharmacyID`)
  REFERENCES `healthcare_1`.`pharmacy` (`pharmacyID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
ADD CONSTRAINT `treatmentID`
  FOREIGN KEY (`treatmentID`)
  REFERENCES `healthcare_1`.`treatment` (`treatmentID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
  
  ALTER TABLE `healthcare_1`.`treatment` 
ADD INDEX `patientID_idx` (`patientID` ASC) VISIBLE,
ADD INDEX `diseaseID_idx` (`diseaseID` ASC) VISIBLE,
ADD INDEX `claimID_idx` (`claimID` ASC) VISIBLE;
;
ALTER TABLE `healthcare_1`.`treatment` 
ADD CONSTRAINT `patientID`
  FOREIGN KEY (`patientID`)
  REFERENCES `healthcare_1`.`patient` (`patientID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
ADD CONSTRAINT `diseaseID`
  FOREIGN KEY (`diseaseID`)
  REFERENCES `healthcare_1`.`disease` (`diseaseID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
ADD CONSTRAINT `claimID`
  FOREIGN KEY (`claimID`)
  REFERENCES `healthcare_1`.`claim` (`claimID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
