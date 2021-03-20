DELETE FROM recipe;
BEGIN;
INSERT INTO recipe (name, fullDescription) VALUES ('Butter Chicken', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Hungarian Goulash', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Simple Chili', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Mexican Beef Rice Casserole', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Balsamic Tomato & Herb Chicken', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Firecracker Meatballs', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Chicken Tikka Masala', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Pad Thai Egg Roll in a Bowl', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Creamy White Chicken Chili', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Zuppa Toscana', 'Super yummy');
INSERT INTO recipe (name, fullDescription) VALUES ('Frozen Pizza', 'Super yummy');
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
