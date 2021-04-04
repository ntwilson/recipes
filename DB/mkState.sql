
BEGIN;
CREATE TABLE appState (
  name text NOT NULL, 
  ingredients text,
  PRIMARY KEY(name)
);
COMMIT;


BEGIN;
INSERT INTO appState (name) VALUES ('input recipes');
COMMIT;
