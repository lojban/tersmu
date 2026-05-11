// JavaScript wrapper for tersmu WASM module
// This file handles the interaction between JavaScript and the WebAssembly module

let wasmMemory = null;
let wasmExports = null;

// Initialize WASM module
async function initWasm(wasmInstance) {
    wasmExports = wasmInstance.exports;
    wasmMemory = wasmExports.memory;
    
    console.log('Available WASM exports:', Object.keys(wasmExports));
    
    // Initialize Haskell RTS if hs_init is available
    if (wasmExports.hs_init) {
        console.log('Initializing Haskell RTS via hs_init(0, 0)...');
        wasmExports.hs_init(0, 0);
    }
    
    // Call our own initializer if available
    if (wasmExports.initTersmu) {
        console.log('Calling initTersmu...');
        wasmExports.initTersmu();
    }
}

// Helper function to allocate memory in WASM
function allocateString(str) {
    const encoder = new TextEncoder();
    const bytes = encoder.encode(str);
    const len = bytes.length;
    
    // Allocate memory (we'll need to implement malloc if not available)
    // For now, assume we have a way to allocate memory
    let ptr;
    if (wasmExports.malloc) {
        ptr = wasmExports.malloc(len + 1);
    } else {
        // Fallback: try to use WASM memory directly
        // This is a simplified approach - actual implementation may vary
        throw new Error('Memory allocation not available. WASM module may need malloc export.');
    }
    
    // Write string to memory
    const memory = new Uint8Array(wasmMemory.buffer, ptr, len + 1);
    memory.set(bytes);
    memory[len] = 0; // null terminator
    
    return { ptr, len };
}

// Helper function to read string from WASM memory
function readString(ptr) {
    const memory = new Uint8Array(wasmMemory.buffer);
    let len = 0;
    while (memory[ptr + len] !== 0 && ptr + len < memory.length) {
        len++;
    }
    const decoder = new TextDecoder();
    return decoder.decode(memory.subarray(ptr, ptr + len));
}

// Main parse function that JavaScript can call
function tersmuParse(input) {
    if (!wasmExports) {
        throw new Error('WASM module not initialized');
    }
    
    // Try to call parseLojban directly if it's exported
    if (wasmExports.parseLojban) {
        try {
            // Allocate input string
            const inputAlloc = allocateString(input);
            
            // Call WASM function
            const resultPtr = wasmExports.parseLojban(inputAlloc.ptr);
            
            // Read result string
            const result = readString(resultPtr);
            
            // Free memory if free is available
            if (wasmExports.free) {
                wasmExports.free(inputAlloc.ptr);
                wasmExports.free(resultPtr);
            }
            
            return result;
        } catch (error) {
            console.error('Error calling WASM parseLojban:', error);
            throw error;
        }
    } else {
        // Alternative: if GHC exports it differently, we need to adapt
        // GHC WASM backend might use a different calling convention
        console.warn('parseLojban not found. Available exports:', Object.keys(wasmExports));
        
        // Try to find any function that might be our parse function
        const possibleNames = ['parseLojban', 'parse_lojban', 'parse', 'hs_parseLojban'];
        for (const name of possibleNames) {
            if (wasmExports[name]) {
                console.log(`Found function: ${name}, trying to use it...`);
                // Try to call it (this may need adjustment based on actual signature)
                try {
                    const inputAlloc = allocateString(input);
                    const resultPtr = wasmExports[name](inputAlloc.ptr);
                    const result = readString(resultPtr);
                    if (wasmExports.free) {
                        wasmExports.free(inputAlloc.ptr);
                        wasmExports.free(resultPtr);
                    }
                    return result;
                } catch (e) {
                    console.error(`Error calling ${name}:`, e);
                }
            }
        }
        
        throw new Error('Could not find parse function in WASM exports. Check WASM build configuration.');
    }
}

// Export for use in HTML
if (typeof window !== 'undefined') {
    window.tersmuParse = tersmuParse;
    window.initWasm = initWasm;
}

// For Node.js environments
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { tersmuParse, initWasm };
}
