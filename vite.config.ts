import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// Use a relative base so the build also works under a GitHub Pages subpath (/<repo>/)
export default defineConfig({
  base: "./",
  plugins: [react()],
});
