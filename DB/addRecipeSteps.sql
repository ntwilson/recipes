ALTER TABLE appState ADD COLUMN recipeSteps TEXT;
CREATE TABLE recipeSteps (
  recipeName TEXT NOT NULL,
  stepNumber SMALLINT NOT NULL,
  stepDescription TEXT NOT NULL,
  PRIMARY KEY(recipeName, stepNumber)
);