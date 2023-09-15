import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { viteCommonjs } from "@originjs/vite-plugin-commonjs";
import { ViteEjsPlugin } from "vite-plugin-ejs";
import { resolve } from "path";

export default defineConfig({
  plugins: [
    viteCommonjs(),
    react(),
    ViteEjsPlugin((config) => {
      return {
        isDev: config.mode === "development",
      };
    }),
  ],
  server: {
    port: 8080,
  },
  build: {
    outDir: "../dist",
  },
  resolve: {
    alias: {
      "@src": resolve(__dirname, "./src"),
      "@test": resolve(__dirname, "./test"),
      "@assets": resolve(__dirname, "./assets"),
    },
  },
  root: "./dev",
});
