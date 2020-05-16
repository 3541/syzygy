include(ExternalProject)

set(KERNEL_RS_LIB "${CMAKE_CURRENT_BINARY_DIR}/${TARGET}-elf/debug/libsyzygy.a")
set(KERNEL_ASM_SOURCES
  src/arch/${TARGET}/boot.asm
  src/arch/${TARGET}/multiboot_header.asm
)

set(KERNEL_SYM "${CMAKE_CURRENT_BINARY_DIR}/kernel.sym")
set(KERNEL_SYM "${KERNEL_SYM}" PARENT_SCOPE)

file(GENERATE OUTPUT linker.ld INPUT src/arch/${TARGET}/linker.ld)
add_custom_target(
  kernel_ld_script
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/linker.ld"
)

ExternalProject_Add(
  kernel_rs
  SOURCE_DIR "${CMAKE_SOURCE_DIR}/kernel"
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${CMAKE_COMMAND} -E env "RUST_TARGET_PATH=${CMAKE_SOURCE_DIR}/targets" "RUSTFLAGS=${RUSTC_FLAGS}" "CARGO_TARGET_DIR=${CMAKE_CURRENT_BINARY_DIR}" xargo build --target "${TARGET}-elf" "${XARGO_FLAGS}" -p syzygy
  BUILD_ALWAYS ON
  INSTALL_COMMAND ""
  USES_TERMINAL_BUILD ON
)

add_custom_command(
  OUTPUT "${KERNEL_RS_LIB}"
  DEPENDS kernel_rs
)

add_executable(kernel ${KERNEL_ASM_SOURCES})
add_dependencies(kernel kernel_rs kernel_ld_script)
target_link_libraries(kernel "${KERNEL_RS_LIB}")
target_link_options(
  kernel
  PRIVATE
  "LINKER:-n,--gc-sections,-T,${CMAKE_CURRENT_BINARY_DIR}/linker.ld"
)

add_custom_command(
  OUTPUT "${KERNEL_SYM}"
  COMMAND "${NM}" --numeric-sort --reverse-sort "$<TARGET_FILE:kernel>" > "${KERNEL_SYM}"
  DEPENDS kernel
)

add_custom_target(
  kernel_sym
  DEPENDS "${KERNEL_SYM}"
)
