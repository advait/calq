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
  root: "./frontend/src",
  build: {
    outDir: "../../dist",
  },
  resolve: {
    alias: {
      "@engine": resolve(__dirname, "./engine"),
      "@purs-compiled": resolve(__dirname, "./output"),
      "@assets": resolve(__dirname, "./frontend/assets"),
    },
  },
});
