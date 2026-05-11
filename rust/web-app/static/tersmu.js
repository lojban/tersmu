import init, { parse_lojban } from './pkg/tersmu.js';

let wasmReady = false;
let wasmInitPromise = null;

export async function initWasm() {
    if (wasmReady) {
        return;
    }
    if (!wasmInitPromise) {
        wasmInitPromise = init().then(() => {
            wasmReady = true;
            console.log('Tersmu Rust WASM module initialized');
        });
    }
    await wasmInitPromise;
}

export function tersmuParse(input) {
    if (!wasmReady) {
        throw new Error('WASM module not initialized');
    }
    return parse_lojban(input);
}

if (typeof window !== 'undefined') {
    window.initWasm = initWasm;
    window.tersmuParse = tersmuParse;
}
