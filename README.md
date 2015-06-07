# SPIR-V

SPIR-V is an IR used by Vulkan, the next generation graphics library that is
the successor to OpenGL. The IR is used as a backend target for languages like
GLSL for OpenGL shaders and OpenCL kernels.

This library acts as an embedded, domain-specific language (EDSL) that is a
variant of SPIR-V in Haskell. The main differences between this EDSL and
regular SPIR-V code is that this library provides significantly relaxed layout
constraints and caching of type and constant declarations.

See the `test` directory for example programs.
