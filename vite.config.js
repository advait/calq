import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { resolve } from "path";

export default defineConfig({
  plugins: [react()],
  server: {
    port: 8080,
  },
  preview: {
    port: 8080,
  },
  root: "./frontend/src",
  build: {
    outDir: "../../dist",
    emptyOutDir: true,
  },
  resolve: {
    alias: {
      "@engine": resolve(import.meta.dirname, "./engine"),
      "@purs-compiled": resolve(import.meta.dirname, "./output"),
      "@assets": resolve(import.meta.dirname, "./frontend/assets"),
    },
  },
});
