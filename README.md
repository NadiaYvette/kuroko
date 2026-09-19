# Kuroko (黒子)

**Unobtrusive, effectful autonomous agent sidecar in modern Haskell.**

> *In classical Japanese theatre, the **kuroko** (黒子) are the stagehands dressed entirely in black who move scenery, handle props, and assist actors while remaining unseen. They never intrude on the drama; they simply ensure the performance never falters.*

`kuroko` is a high-assurance, compact autonomous coding assistant and agent runtime designed as a clean-slate alternative to monolithic agent frameworks (such as `intelli-monad`). By assembling modern, high-abstraction Hackage packages, `kuroko` achieves complete agent capabilities in **under 1,500 lines of Haskell**.

---

## Architecture & The 5 Pillars

```
                                  ┌───────────────────────────────┐
                                  │         agent.dhall           │
                                  │ (Typed Policies, Prompts,     │
                                  │  Budgets & Tool Allowlists)   │
                                  └───────────────┬───────────────┘
                                                  │
                                  ┌───────────────▼───────────────┐
                                  │    Kuroko.Workflow.ReAct      │
                                  │  (Porcupine / Arrow Flow DAG  │
                                  │   with CAS-Store Step Cache)  │
                                  └───────────────┬───────────────┘
                                                  │
                  ┌───────────────────────────────┼───────────────────────────────┐
                  ▼                               ▼                               ▼
      ┌───────────────────────┐       ┌───────────────────────┐       ┌───────────────────────┐
      │   Kuroko.Effect.LLM   │       │  Kuroko.Effect.Tool   │       │  Kuroko.Effect.Store  │
      ├───────────────────────┤       ├───────────────────────┤       ├───────────────────────┤
      │ • ChatCompletion      │       │ • Vinyl / DocRec rows │       │ • persistent-effectful│
      │ • StreamTokens        │       │ • Auto-JSON-Schema    │       │ • SQLite WAL Journal  │
      │ • Handlers:           │       │ • Dynamic dispatch    │       │ • Checkpointing &     │
      │   OpenAI, Claude, Mock│       │ • MCP Server Bridge   │       │   Review Seam Log     │
      └───────────────────────┘       └───────────────────────┘       └───────────────────────┘
```

1. **LLM as an Algebraic Effect (`Kuroko.Effect.LLM`)**:
   - Swappable handlers for live OpenAI / Anthropic / local Ollama endpoints via HTTP streaming.
   - Deterministic offline mock interpreter (`Kuroko.Effect.LLM.Mock`) for zero-network unit testing and CI regression suites.
2. **Extensible Record Tools (`Kuroko.Effect.Tool`)**:
   - Built on `vinyl` and `porcupine`'s `DocRecord`. Tool schemas and docstrings are typed records; JSON Schema is automatically derived.
3. **Durable Persistence (`Kuroko.Effect.Store`)**:
   - Built atop `persistent-effectful` and SQLite WAL. Sessions, messages, and tool audits are durable append-only event streams.
4. **Hermetic Configuration (`Kuroko.Config.Dhall`)**:
   - Typed agent configurations, safety policies, and model parameters evaluated via Dhall.
5. **Native MCP Support (`Kuroko.Effect.MCP`)**:
   - Exposes tools over stdio JSON-RPC 2.0 to any Model Context Protocol host (Cursor, Zed, Claude Desktop).

---

## Directory Layout

```
src/
├── Kuroko/
│   ├── Core/
│   │   ├── Types.hs         -- Message, Role, Token, ToolCall, ToolDef
│   │   └── Schema.hs        -- JSON Schema builders
│   ├── Effect/
│   │   ├── LLM.hs           -- LLM effect definition
│   │   ├── LLM/Mock.hs      -- Deterministic mock handler
│   │   ├── LLM/OpenAI.hs    -- OpenAI / Claude HTTP streaming handler
│   │   ├── Tool.hs          -- Tool execution effect & registry
│   │   ├── Store.hs         -- Persistent SQLite session & audit store
│   │   └── MCP.hs           -- Stdio Model Context Protocol (MCP) server
│   ├── Workflow/
│   │   └── ReAct.hs         -- ReAct reasoning & tool execution loop
│   └── Config/
│       └── Dhall.hs         -- Typed Dhall configuration loader
└── Kuroko.hs                -- Top-level re-exports

app/
└── Main.hs                  -- CLI runner / REPL / MCP server
test/
├── Spec.hs
└── MockSpec.hs              -- 100% offline agent test suite
config/
└── agent.dhall              -- Example Dhall configuration
```

---

## Quickstart

### Build

```bash
cabal build
```

### Run Tests

```bash
cabal test
```

### Run the ReAct Loop

```bash
kuroko run -c config/agent.dhall "Inspect the directory structure"
```

### Serve as an MCP Server

```bash
kuroko mcp
```
