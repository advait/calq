/**
 * Registers the custom loader so node can resolve the "@engine" and "?raw" imports in tests.
 */
import { registerHooks } from "node:module";
import { load, resolve } from "./custom-loader.mjs";

registerHooks({ resolve, load });
