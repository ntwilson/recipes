DELETE FROM recipe;
BEGIN;
INSERT INTO recipe (name, fullDescription) VALUES (
  'Butter Chicken',
  'Super yummy'
);
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
