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
5. **Native MCP Support (`Kuroko.Effect.MCP` & `Kuroko.Server.MCP`)**:
   - Stdio transport for local CLI agent workflows.
   - HTTP/SSE transport built with `servant-effectful` running natively inside `Eff es` without unlifting ceremony.
6. **Servant-Client & REST API (`Kuroko.Effect.LLM.ServantClient` & `Kuroko.Server.API`)**:
   - Typed `servant-client` for OpenAI/Claude endpoints (~90 LOC replacing 3,000 LOC of OpenAPI generators).
   - Agent REST management API exposing `/api/v1/react` and `/api/v1/tools` with automatic OpenAPI derivation.

---

## Directory Layout

```
src/
├── Kuroko/
│   ├── Core/
│   │   ├── Types.hs             -- Message, Role, Token, ToolCall, ToolDef
│   │   └── Schema.hs            -- JSON Schema builders
│   ├── Effect/
│   │   ├── LLM.hs               -- LLM effect definition
│   │   ├── LLM/Mock.hs          -- Deterministic mock handler
│   │   ├── LLM/OpenAI.hs        -- Raw HTTP streaming client
│   │   ├── LLM/ServantClient.hs -- Typed Servant-Client LLM handler
│   │   ├── Tool.hs              -- Tool execution effect & registry
│   │   ├── Store.hs             -- Persistent SQLite session & audit store
│   │   └── MCP.hs               -- Stdio Model Context Protocol (MCP) server
│   ├── Server/
│   │   ├── MCP.hs               -- Servant-effectful HTTP/SSE MCP server
│   │   └── API.hs               -- Servant-effectful Agent REST API
│   ├── Workflow/
│   │   └── ReAct.hs             -- ReAct reasoning & tool execution loop
│   └── Config/
│       └── Dhall.hs             -- Typed Dhall configuration loader
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
