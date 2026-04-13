-- This is for a PostgreSQL database
-- Database: microloggers
-- Note: all timestamps must be WITH timezone, otherwise R has trouble

-- Database might already exist from initial setup
-- CREATE DATABASE microloggers;

-- Table: groups

DROP TABLE IF EXISTS groups CASCADE;

CREATE TABLE groups
(
    group_id TEXT PRIMARY KEY,
    group_desc TEXT
);

-- Table: structures

DROP TABLE IF EXISTS structures CASCADE;

CREATE TABLE structures
(
    type TEXT NOT NULL,
    version INTEGER NOT NULL,
    tree_json TEXT,
    PRIMARY KEY (type, version)
);

-- Table: devices

DROP TABLE IF EXISTS devices CASCADE;

CREATE TABLE devices
(
    core_id TEXT PRIMARY KEY,
    group_id TEXT NOT NULL REFERENCES groups(group_id),
    published_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    type TEXT,
    version INTEGER,
    values_json TEXT
);

-- Table: experiments

DROP TABLE IF EXISTS experiments CASCADE;

CREATE TABLE experiments
(
  exp_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  group_id TEXT NOT NULL REFERENCES groups(group_id),
  user_id TEXT NOT NULL,
  user_first TEXT,
  user_last TEXT,
  name TEXT,
  description TEXT,
  notes TEXT,
  recording BOOLEAN NOT NULL DEFAULT FALSE,
  current_segment INTEGER NOT NULL DEFAULT 0,
  last_recording_change TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  archived BOOLEAN NOT NULL DEFAULT FALSE
);

-- Table: experiment_devices

DROP TABLE IF EXISTS experiment_devices CASCADE;

CREATE TABLE experiment_devices (
  exp_id INTEGER NOT NULL REFERENCES experiments(exp_id),
  core_id TEXT NOT NULL REFERENCES devices(core_id),
  label TEXT,
  UNIQUE (exp_id, core_id)
);

-- Table: device_snapshots

DROP TABLE IF EXISTS device_snapshots CASCADE;

CREATE TABLE device_snapshots (
  snapshot_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  core_id TEXT NOT NULL REFERENCES devices(core_id),
  snapshot_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
  snapshot_json TEXT
);

-- Table: logs

DROP TABLE IF EXISTS logs CASCADE;

CREATE TABLE logs (
  log_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  core_id TEXT NOT NULL REFERENCES devices(core_id),
  core_name TEXT,
  log_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
  log_time_offset REAL DEFAULT 0.0,
  data_path TEXT NOT NULL,
  data_text TEXT,
  data_value DOUBLE PRECISION,
  data_n INTEGER,
  data_sd DOUBLE PRECISION,
  data_units TEXT
);

-- Table: experiment_logs

DROP TABLE IF EXISTS experiment_logs CASCADE;

CREATE TABLE experiment_logs (
  exp_id INTEGER NOT NULL REFERENCES experiments(exp_id),
  segment INTEGER NOT NULL,
  log_id INTEGER NOT NULL REFERENCES logs(log_id),
  UNIQUE (exp_id, log_id)
);

