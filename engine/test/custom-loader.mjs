/**
 * Custom node module loader for Spago that handles ESM raw string modules. Note that this
 * isn't required for production as vite understands these import statements natively.
 */
import { readFileSync } from "fs";
import path from "path";
import { fileURLToPath, URL } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, "../../");

/**
 * Resolves custom specifiers, e.g., "@engine" and "?raw".
 */
export function resolve(specifier, context, nextResolve) {
  let resolvedPath = specifier;
  let isCustomPath = false;

  if (specifier.startsWith("@engine")) {
    resolvedPath = path.join(projectRoot, "./engine", specifier.substring(8));
    isCustomPath = true;
  }

  if (specifier.endsWith("?raw")) {
    resolvedPath = resolvedPath.replace(/\?raw$/, "");
    isCustomPath = true;
  }

  const resolvedUrl = context.parentURL
    ? new URL(resolvedPath, context.parentURL).href
    : new URL(resolvedPath).href;

  return isCustomPath
    ? { shortCircuit: true, url: resolvedUrl }
    : nextResolve(specifier, context);
}

/**
 * Custom loader that reads .calq files as raw string modules.
 */
export async function load(url, context, nextLoad) {
  if (url.endsWith(".calq")) {
    const content = readFileSync(fileURLToPath(url), "utf-8");
    return {
      format: "module",
      shortCircuit: true,
      source: `export default ${JSON.stringify(content)}`,
    };
  }

  return nextLoad(url, context);
}
