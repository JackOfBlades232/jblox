#include <os.h>
#include <memory.h>
#include <buffer.h>
#include <io.h>
#include <logging.h>
#include <defs.h>
#include <gpa.h>
#include <debug.h>

#include "lox.c"

static int common_main(void)
{
    buffer_t program_memory = buf_allocate_best(1ull << 30);
    VERIFY(buf_is_valid(&program_memory), "Failed to allocate program memory.");

    g_gpa = gpa_make(program_memory);

    return lox_main();
}


#if _WIN32

// Ugh
int _fltused=0;

typedef struct win32_list_entry {
    struct win32_list_entry *flink;
    struct win32_list_entry *blink;
} win32_list_entry_t;

typedef struct {
    win32_list_entry_t linked_list;
    win32_list_entry_t unused_list;
    void *base_address;
    void *reserved2[1];
    void *dll_base;
    void *entry_point;
    void *reserved3;
    ushort dll_name_length;
    ushort dll_name_maximum_length;
    wchar *dll_name_buffer;
} win32_ldr_data_entry_t;

typedef struct {
    u8 pad_[0x20];
    win32_ldr_data_entry_t *data_entry;
} win32_ldr_t;

typedef struct {
    u8 pad_[0x18];
    win32_ldr_t *ldr;
} win32_peb_t;

typedef struct {
    u8 pad_[0x3c];
    u32 pe_offset;
} win32_msdos_t;

typedef struct {
    u32 virtual_address;
    u32 size;
} win32_pe_image_data_t;

typedef struct {
    // COFF
    u8 signature[4];
    u16 machine;
    u16 number_of_sections;
    u32 time_date_stamp;
    u32 pointer_to_symbol_table;
    u32 number_of_symbols;
    u16 size_of_optional_header;
    u16 characteristics;

    // Assuming PE32+ Optional Header since this is 64bit only
    // standard fields
    u16 magic; 
    u8 major_linker_version;
    u8 minor_linker_version;
    u32 size_of_code;
    u32 size_of_initialized_data;
    u32 size_of_uninitialized_data;
    u32 address_of_entry_point;
    u32 base_of_code;

    // windows specific fields
    u64 image_base;
    u32 section_alignment;
    u32 file_alignment;
    u16 major_operating_system_version;
    u16 minor_operating_system_version;
    u16 major_image_version;
    u16 minor_image_version;
    u16 major_subsystem_version;
    u16 minor_subsystem_version;
    u32 win32_version_value;
    u32 size_of_image;
    u32 size_of_headers;
    u32 check_sum;
    u16 subsystem;
    u16 dll_characteristics;
    u64 size_of_stack_reserve;
    u64 size_of_stack_commit;
    u64 size_of_heap_reserve;
    u64 size_of_heap_commit;
    u32 loader_flags;
    u32 number_of_rva_and_sizes;

    // data directories
    win32_pe_image_data_t export_table;
    win32_pe_image_data_t import_table;
    win32_pe_image_data_t resource_table;
    win32_pe_image_data_t exception_table;
    win32_pe_image_data_t certificate_table;
    win32_pe_image_data_t baseRelocation_table;
    win32_pe_image_data_t debug;
    win32_pe_image_data_t architecture;
    win32_pe_image_data_t global_ptr;
    win32_pe_image_data_t tls_table;
    win32_pe_image_data_t load_config_table;
    win32_pe_image_data_t bound_import;
    win32_pe_image_data_t iat;
    win32_pe_image_data_t delay_import_descriptor;
    win32_pe_image_data_t clr_runtime_header;
    win32_pe_image_data_t reserved_table;
} win32_pe_t;

typedef struct {
    u32 export_flags;
    u32 time_date_stamp;
    u16 major_version;
    u16 minor_version;
    u32 name_rva;
    u32 ordinal_base;
    u32 address_table_entries;
    u32 number_of_name_pointers;
    u32 export_address_table_rva;
    u32 name_pointer_rva;
    u32 ordinal_table_rva;
} win32_pe_export_table_t;

static inline b32
win32_compare_name_case_sensitive_a(char const *l, char const *r)
{
    while (*l && *r && *l == *r)
    {
        ++l;
        ++r;
    }
    return !*l && !*r;
}

static inline b32 win32_compare_name_case_insensitive_sized_w(
    wchar const *l, wchar const *r, usize cnt)
{
    while (cnt && (*l == *r || ABS(*l - *r) == L'a' - L'A'))
    {
        --cnt;
        ++l;
        ++r;
    }
    return cnt == 0;
}

static inline win32_get_proc_addr_t
win32_get_get_proc_addr(win32_handle_t kernel32)
{
#define WIN32_PE_GET_OFFSET(mod_, off_, type_) \
    (type_ const *)((u8 *)(mod_) + (off_))

    win32_msdos_t const *msdos_header =
        WIN32_PE_GET_OFFSET(kernel32, 0, win32_msdos_t);
    win32_pe_t const *pe_header =
        WIN32_PE_GET_OFFSET(kernel32, msdos_header->pe_offset, win32_pe_t);
    win32_pe_export_table_t const *export_table =
        WIN32_PE_GET_OFFSET(
            kernel32,
            pe_header->export_table.virtual_address,
            win32_pe_export_table_t);
    u32 const *name_ptr_table =
        WIN32_PE_GET_OFFSET(kernel32, export_table->name_pointer_rva, u32);
    u32 id = ((u32)-1);
    for (u32 i = 0; i < export_table->number_of_name_pointers; ++i) {
        char const *proc_name =
            WIN32_PE_GET_OFFSET(kernel32, name_ptr_table[i], char);
        if (win32_compare_name_case_sensitive_a(proc_name, "GetProcAddress")) {
            id = i;
            break;
        }
    }
    if (id == ((u32)-1))
        return NULL;

    u16 const *ordinal_table =
        WIN32_PE_GET_OFFSET(kernel32, export_table->ordinal_table_rva, u16);
    u16 get_proc_addr_ordinal = ordinal_table[id];

    u32 const *export_address_table =
        WIN32_PE_GET_OFFSET(
            kernel32, export_table->export_address_table_rva, u32);
    u32 get_proc_addr_rva = export_address_table[get_proc_addr_ordinal];

    return (win32_get_proc_addr_t)WIN32_PE_GET_OFFSET(
        kernel32, get_proc_addr_rva, void);

#undef WIN32_PE_GET_OFFSET
}

static inline win32_handle_t win32_get_kernel32_mod(win32_peb_t const *peb)
{
    wchar const kernel32_name[] = L"kernel32.dll";
    usize const kernel32_name_size = STRLITLEN(kernel32_name);
    win32_ldr_data_entry_t const *ldr_entry = peb->ldr->data_entry;
    while (ldr_entry->dll_base) {
        if (win32_compare_name_case_insensitive_sized_w(
            ldr_entry->dll_name_buffer,
            kernel32_name,
            MIN(ldr_entry->dll_name_length, kernel32_name_size)))
        {
            return (win32_handle_t)ldr_entry->base_address;
        }

        ldr_entry =
            (win32_ldr_data_entry_t const *)ldr_entry->linked_list.flink;
    }

    return NULL;
}

static inline win32_peb_t const *win32_get_peb_x64(void)
{
    return (win32_peb_t const *)__readgsqword(0x60);
}

static inline void win32_try_enable_large_pages(win32_handle_t kernel32)
{
#define WIN32_CHECK(e_)                                     \
    do                                                      \
    {                                                       \
        if (!(e_)) {                                        \
            if (advapi32)                                   \
                g_os.sys.close_handle(advapi32);            \
            return;                                         \
        }                                                   \
    } while (0)
    win32_handle_t advapi32 = g_os.sys.load_library_a("advapi32.dll");
    WIN32_CHECK(advapi32);

    win32_open_process_token_t open_process_token =
        (win32_open_process_token_t)g_os.sys.get_proc_addr(
            advapi32, "OpenProcessToken");
    WIN32_CHECK(open_process_token);

    win32_lookup_privilege_value_a_t lookup_privilege_value_a =
        (win32_lookup_privilege_value_a_t)g_os.sys.get_proc_addr(
                advapi32, "LookupPrivilegeValueA");
    WIN32_CHECK(lookup_privilege_value_a);
    win32_adjust_token_privileges_t adjust_token_privileges =
        (win32_adjust_token_privileges_t)g_os.sys.get_proc_addr(
                advapi32, "AdjustTokenPrivileges");
    WIN32_CHECK(adjust_token_privileges);

    win32_get_large_page_minimum_t get_large_page_minimum =
        (win32_get_large_page_minimum_t)g_os.sys.get_proc_addr(
                kernel32, "GetLargePageMinimum");
    WIN32_CHECK(get_large_page_minimum);

    win32_handle_t token_hnd;

    if (open_process_token(
        g_os.process_hnd, WIN32_TOKEN_ADJUST_PRIVILEGES, &token_hnd))
    {
        win32_token_privileges_t privs = {0};
        privs.privilege_count = 1;
        privs.privileges[0].attributes = WIN32_SE_PRIVILEGE_ENABLED;
        if (lookup_privilege_value_a(
            NULL, "SeLockMemoryPrivilege", &privs.privileges[0].luid)) 
        {
            adjust_token_privileges(token_hnd, false, &privs, 0, NULL, NULL);
            if (g_os.sys.get_last_error() == WIN32_ERROR_SUCCESS)
                g_os.large_page_size = get_large_page_minimum();
        }
        
        g_os.sys.close_handle(token_hnd);
    }


    g_os.sys.free_library(advapi32);
}

static void win32_main(void)
{
#define WIN32_NO_MANS_LAND_VERIFY(e_) \
    do { if(!(e_)) *(int volatile *)NULL = 0; } while (0)

    win32_peb_t const *peb = win32_get_peb_x64();
    WIN32_NO_MANS_LAND_VERIFY(peb);

    win32_handle_t kernel32 = win32_get_kernel32_mod(peb);
    WIN32_NO_MANS_LAND_VERIFY(kernel32);

    WIN32_NO_MANS_LAND_VERIFY(
        g_os.sys.get_proc_addr = win32_get_get_proc_addr(kernel32));

    WIN32_NO_MANS_LAND_VERIFY(
        g_os.sys.exit_process =
            (win32_exit_process_t)g_os.sys.get_proc_addr(
                    kernel32, "ExitProcess"));

#undef WIN32_NO_MANS_LAND_PANIC
#define WIN32_BOOT_VERIFY(e_) \
    do { if(!(e_)) g_os.sys.exit_process(4221); } while (0)

    WIN32_BOOT_VERIFY(
        g_os.sys.load_library_a =
            (win32_load_library_a_t)g_os.sys.get_proc_addr(
                    kernel32, "LoadLibraryA"));
    WIN32_BOOT_VERIFY(
        g_os.sys.free_library =
            (win32_free_library_t)g_os.sys.get_proc_addr(
                    kernel32, "FreeLibrary"));
    WIN32_BOOT_VERIFY(
        g_os.sys.get_last_error =
            (win32_get_last_error_t)g_os.sys.get_proc_addr(
                    kernel32, "GetLastError"));
    WIN32_BOOT_VERIFY(
        g_os.sys.close_handle =
            (win32_close_handle_t)g_os.sys.get_proc_addr(
                    kernel32, "CloseHandle"));
    WIN32_BOOT_VERIFY(
        g_os.sys.virtual_alloc =
            (win32_virtual_alloc_t)g_os.sys.get_proc_addr(
                    kernel32, "VirtualAlloc"));
    WIN32_BOOT_VERIFY(
        g_os.sys.virtual_free =
            (win32_virtual_free_t)g_os.sys.get_proc_addr(
                    kernel32, "VirtualFree"));
    WIN32_BOOT_VERIFY(
        g_os.sys.create_file_a =
            (win32_create_file_a_t)g_os.sys.get_proc_addr(
                    kernel32, "CreateFileA"));
    WIN32_BOOT_VERIFY(
        g_os.sys.write_file =
            (win32_write_file_t)g_os.sys.get_proc_addr(
                    kernel32, "WriteFile"));
    WIN32_BOOT_VERIFY(
        g_os.sys.read_file =
            (win32_read_file_t)g_os.sys.get_proc_addr(
                    kernel32, "ReadFile"));

    win32_get_current_process_t get_current_process = 
        (win32_get_current_process_t)g_os.sys.get_proc_addr(
                kernel32, "GetCurrentProcess");
    WIN32_BOOT_VERIFY(get_current_process);

    win32_get_std_handle_t get_std_handle = 
        (win32_get_std_handle_t)g_os.sys.get_proc_addr(
                kernel32, "GetStdHandle");
    WIN32_BOOT_VERIFY(get_std_handle);

    // @TODO: calls through loaded fptrs
    g_os.process_hnd = get_current_process();
    g_os.regular_page_size = 4096;

    win32_try_enable_large_pages(kernel32);

    g_os.hstdin = (io_handle_t){get_std_handle(WIN32_STD_INPUT_HANDLE)};
    g_os.hstdout = (io_handle_t){get_std_handle(WIN32_STD_OUTPUT_HANDLE)};
    g_os.hstderr = (io_handle_t){get_std_handle(WIN32_STD_ERROR_HANDLE)};

    WIN32_BOOT_VERIFY(g_os.hstdin.hnd != WIN32_INVALID_HANDLE_VALUE);
    WIN32_BOOT_VERIFY(g_os.hstdout.hnd != WIN32_INVALID_HANDLE_VALUE);
    WIN32_BOOT_VERIFY(g_os.hstderr.hnd != WIN32_INVALID_HANDLE_VALUE);

#undef WIN32_BOOT_VERIFY

    {
        win32_handle_t shell32 = g_os.sys.load_library_a("shell32.dll");
        VERIFY(shell32, "Failed to load \"shell32.dll\", required for argv.");
        win32_get_command_line_w_t get_command_line_w = 
            (win32_get_command_line_w_t)g_os.sys.get_proc_addr(
                    kernel32, "GetCommandLineW");
        VERIFY(get_command_line_w,
            "Failed to load \"GetCommandLineW\" from \"kernel32.dll\"");
        win32_command_line_to_argv_w_t command_line_to_argv_w = 
            (win32_command_line_to_argv_w_t)g_os.sys.get_proc_addr(
                    shell32, "CommandLineToArgvW");
        VERIFY(command_line_to_argv_w,
            "Failed to load \"CommandLineToArgvW\" from \"shell32.dll\"");

        int argc;
        wchar **wargv = command_line_to_argv_w(get_command_line_w(), &argc);

        for (int i = 0; i < argc; ++i) {
            wchar const *warg = wargv[i];
            char *arg = (char *)(wargv[i]);
            while (*warg) {
                VERIFY(
                    *warg <= 0xFF, "Only ascii characters supported in argv.");
                *arg++ = (char)(*warg++);
            }
            *arg = '\0';
        }
    
        g_os.argc = argc;
        g_os.argv = (char **)wargv;

        g_os.sys.free_library(shell32);
    }

    g_os.sys.exit_process((uint)common_main());
}

void start(void)
{
    win32_main();    
}

#else

static int sys_main(int argc, char **argv)
{
    g_os.argc = argc;
    g_os.argv = argv;
    g_os.pid = sys_getpid();
    g_os.regular_page_size = 4096; // @TODO: support properly?
    fmt_sprint(
        g_os.stat_file_name_buf, sizeof(g_os.stat_file_name_buf),
        "/proc/%d/stat", g_os.pid);
    g_os.hstdin = (io_handle_t){SYS_STDIN_FILENO + 1};
    g_os.hstdout = (io_handle_t){SYS_STDOUT_FILENO + 1};
    g_os.hstderr = (io_handle_t){SYS_STDERR_FILENO + 1};

    return common_main();
}

__attribute__((naked)) void _start(void)
{
    asm(
        "xor rbp, rbp\n"
        "pop rdi\n"
        "mov rsi, rsp\n"
        "and rsp, -16\n"
        "call sys_main\n"
        "mov rdi, rax\n"
        "mov rax, 60\n"
        "syscall\n"
        "ret");
}

#endif
