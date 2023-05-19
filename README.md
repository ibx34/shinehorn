# Shinehorn ✨📯

A strongly typed lisp language intended for use in configuration. Shinehorn config files **must** be accompanied by a matching definitions file ([example](/definitions.shinyd)). A definition file defines all the required and optional fields as well as their types. Once one of these is created you can use Shinehorn normally (no example rightnow).

# Language Specification

Directives:
- Directives do just what the name says, directs. They tell the parser and compiler how to handle the actual Shinehorn config files.
    - **define** (`(#define)`): Defines variable (or fields, however you wanna call them) types.
        - ```(#define database_url String)```  