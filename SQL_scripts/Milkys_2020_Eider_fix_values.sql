
-- Used in script 812 (2020) in Milkys2_pc  

-- IMPORTANT: change FLAG1 first 
UPDATE NIVADATABASE.BIOTA_CHEMISTRY_VALUES
SET FLAG1 = '<' WHERE VALUE_ID >= 2004793 and VALUE_ID <= 2006914 and VALUE < 0;

-- Then change VALUE
UPDATE NIVADATABASE.BIOTA_CHEMISTRY_VALUES
SET VALUE = (-VALUE) WHERE VALUE_ID >= 2004793 and VALUE_ID <= 2006914 and VALUE < 0;

commit;