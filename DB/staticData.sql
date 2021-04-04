DELETE FROM recipe;
BEGIN;
INSERT INTO recipe (name) VALUES ('Butter Chicken');
INSERT INTO recipe (name) VALUES ('Hungarian Goulash');
INSERT INTO recipe (name) VALUES ('Simple Chili');
INSERT INTO recipe (name) VALUES ('Mexican Beef Rice Casserole');
INSERT INTO recipe (name) VALUES ('Balsamic Tomato & Herb Chicken');
INSERT INTO recipe (name) VALUES ('Firecracker Meatballs');
INSERT INTO recipe (name) VALUES ('Chicken Tikka Masala');
INSERT INTO recipe (name) VALUES ('Pad Thai Egg Roll in a Bowl');
INSERT INTO recipe (name) VALUES ('Creamy White Chicken Chili');
INSERT INTO recipe (name) VALUES ('Zuppa Toscana');
INSERT INTO recipe (name) VALUES ('Frozen Pizza');
COMMIT;

DELETE FROM ingredient;
BEGIN;
INSERT INTO ingredient (name, store, section, common) VALUES (
  'heavy cream',
  'ALDI',
  'Dairy',
  true
);
INSERT INTO ingredient (name, store, section, common) VALUES (
  'chicken',
  'ALDI',
  'Meat',
  false
);
COMMIT;

DELETE FROM recipeIngredients;
BEGIN;
INSERT INTO recipeIngredients (recipe, ingredient, quantity, units) VALUES (
  'Butter Chicken',
  'heavy cream',
  0.5,
  'cups'
);
INSERT INTO recipeIngredients (recipe, ingredient, quantity, units) VALUES (
  'Butter Chicken',
  'chicken',
  2,
  'lbs'
);
COMMIT;


DELETE FROM settings;
BEGIN;
INSERT INTO settings (name, value) VALUES (
  'schemaVersion',
  '1.0'
);
COMMIT;

BEGIN;
INSERT INTO appState (name) VALUES ('input recipes');
COMMIT;
