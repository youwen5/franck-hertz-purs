import {
  runFranckHertzExperiment,
  createDefaultState,
  createDefaultRandomState,
  stepSimulation,
} from "./index.js";

// Run the full experiment simulation
//console.log("Running full Franck-Hertz experiment simulation:");
//const fullExperiment = runFranckHertzExperiment(1.5)(0.0)(30.0)(0.5)(3.5)(500)(12345);
//console.log(fullExperiment);

// Demo of running the simulation with individual steps
console.log("\nRunning individual steps simulation demo:");

// Create initial state and setup
let state = createDefaultState;
let randomSeed = 12345;

// Set custom values for the experiment
state.settings.filamentVoltage = 6.0;
state.settings.acceleratingVoltage = 10.0; // Use a fixed voltage for demonstration
state.mercuryDensity = 1.0;

// Run 50 individual steps and log the data every 10 steps
console.log(
  "Starting step-by-step simulation with accelerating voltage:",
  state.settings.acceleratingVoltage,
);
const dataPoints = [];

for (let i = 0; i < 2900; i++) {
  // Perform a single simulation step
  const result = stepSimulation(state)(randomSeed);
  // Update the state and random seed for the next iteration
  state = result.newState;
  randomSeed = result.randomSeed;

  // Log data every 10 steps
  if (i % 10 === 0) {
    dataPoints.push({
      step: i,
      electronCount: state.electrons.length,
      collectedCount: state.collectedCount,
      currentReading: state.currentReading,
    });

    console.log(
      `Step ${i}: ${state.electrons.length} electrons, ${state.collectedCount} collected, Current: ${state.currentReading}`,
    );
  }
}

console.log("\nFinal state after 50 steps:");
console.log({
  totalElectrons: state.electrons.length,
  collectedCount: state.collectedCount,
  currentReading: state.currentReading,
  timeStep: state.timeStep,
});
