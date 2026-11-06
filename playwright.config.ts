import { defineConfig, devices } from "@playwright/test";

const projects = [
  { name: "chromium", use: { ...devices["Desktop Chrome"] } },
  { name: "mobile-chrome", use: { ...devices["Pixel 7"] } },
];

export default defineConfig({
  testDir: "e2e",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  reporter: process.env.CI ? [["github"], ["list"]] : "list",
  use: { baseURL: "http://localhost:8080" },
  // Tests run against the production bundle in dist/ (run `pnpm build` first).
  webServer: {
    command: "node node_modules/vite/bin/vite.js preview --strictPort",
    url: "http://localhost:8080",
    reuseExistingServer: !process.env.CI,
  },
  // E2E_PROJECTS=chromium narrows the run to one project.
  projects: projects.filter(
    (p) =>
      !process.env.E2E_PROJECTS ||
      process.env.E2E_PROJECTS.split(",").includes(p.name),
  ),
});
