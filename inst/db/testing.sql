-- This creates data for testing purposes

-- Table: groups
INSERT INTO groups(group_id, group_desc) VALUES ('TEST', 'Testing group');

-- Table: devices
INSERT INTO devices(group_id, core_id) VALUES ('TEST', 'test1');

-- Table: experiments
INSERT INTO experiments(exp_id, group_id, user_id, recording) 
OVERRIDING SYSTEM VALUE
VALUES (1, 'TEST', 'testuser', True), (2, 'TEST', 'testuser', True), (3, 'TEST', 'testuser', False);

-- Table: experiment_devices
INSERT INTO experiment_devices(exp_id, core_id)
VALUES(1, 'test1'), (2, 'test1'), (3, 'test1');
