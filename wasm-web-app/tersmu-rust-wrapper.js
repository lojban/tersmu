// JavaScript wrapper for Rust WASM module (wasm-bindgen generated)
// This file provides a unified interface for the Rust implementation

let rustWasmModule = null;

// Initialize Rust WASM module
async function initRustWasm() {
    try {
        // Import the wasm-bindgen generated module
        const module = await import('./tersmu-rust.js');
        await module.default(); // Initialize the WASM module
        rustWasmModule = module;
        console.log('Rust WASM module initialized');
        return true;
    } catch (error) {
        console.error('Error loading Rust WASM:', error);
        throw error;
    }
}

// Parse function that matches the Haskell interface
function tersmuParseRust(input) {
    if (!rustWasmModule) {
        throw new Error('Rust WASM module not initialized');
    }

    try {
        // Call the Rust parse_lojban function
        const result = rustWasmModule.parse_lojban(input);
        return result;
    } catch (error) {
        console.error('Error calling Rust WASM parse_lojban:', error);
        throw error;
    }
}

// Export for use in HTML
if (typeof window !== 'undefined') {
    window.initRustWasm = initRustWasm;
    window.tersmuParseRust = tersmuParseRust;
}
