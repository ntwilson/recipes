CREATE TABLE recipe (
  name text NOT NULL, 
  PRIMARY KEY(name)
);

CREATE TABLE ingredient (
  name text NOT NULL,
  store text NOT NULL,
  section text,
  common boolean NOT NULL,
  PRIMARY KEY(name)
);

CREATE TABLE recipeIngredients (
  recipe text NOT NULL,
  ingredient text NOT NULL,
  quantity double precision NOT NULL,
  units text,
  PRIMARY KEY(recipe, ingredient)
);

CREATE TABLE appState (
  name text NOT NULL, 
  ingredients text,
  recipeSteps text,
  PRIMARY KEY(name)
);

CREATE TABLE recipeSteps (
  recipeName TEXT NOT NULL,
  stepNumber SMALLINT NOT NULL,
  stepDescription TEXT NOT NULL,
  PRIMARY KEY(recipeName, stepNumber)
);
