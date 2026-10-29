!> OpenGL 3.3 core / GLES 3.0 renderer backend.
!>
!> One shader program, one VAO, one VBO, one R8 atlas, one draw call.
!> Entry points are resolved through a host supplied resolver
!> (SDL_GL_GetProcAddress / glfwGetProcAddress / eglGetProcAddress), so this
!> module links no GL library and touches no platform API.
module ui_gfx_gl
   use, intrinsic :: iso_c_binding
   use ui_kinds
   use ui_geom
   use ui_gfx
   implicit none
   private

   public :: ui_gl_backend, gl_init, gl_null

   integer(c_int), parameter :: GL_TRIANGLES = int(z'0004', c_int)
   integer(c_int), parameter :: GL_UNSIGNED_BYTE = int(z'1401', c_int)
   integer(c_int), parameter :: GL_FLOAT = int(z'1406', c_int)
   integer(c_int), parameter :: GL_COLOR_BUFFER_BIT = int(z'4000', c_int)
   integer(c_int), parameter :: GL_TEXTURE_2D = int(z'0DE1', c_int)
   integer(c_int), parameter :: GL_TEXTURE0 = int(z'84C0', c_int)
   integer(c_int), parameter :: GL_TEXTURE1 = int(z'84C1', c_int)
   integer(c_int), parameter :: GL_RGBA_C = int(z'1908', c_int)
   integer(c_int), parameter :: GL_R8 = int(z'8229', c_int)
   integer(c_int), parameter :: GL_RED = int(z'1903', c_int)
   integer(c_int), parameter :: GL_NEAREST = int(z'2600', c_int)
   integer(c_int), parameter :: GL_LINEAR_F = int(z'2601', c_int)
   integer(c_int), parameter :: GL_NEAREST_F = int(z'2600', c_int)
   integer(c_int), parameter :: GL_TEXTURE_MAG_FILTER = int(z'2800', c_int)
   integer(c_int), parameter :: GL_TEXTURE_MIN_FILTER = int(z'2801', c_int)
   integer(c_int), parameter :: GL_TEXTURE_WRAP_S = int(z'2802', c_int)
   integer(c_int), parameter :: GL_TEXTURE_WRAP_T = int(z'2803', c_int)
   integer(c_int), parameter :: GL_CLAMP_TO_EDGE = int(z'812F', c_int)
   integer(c_int), parameter :: GL_UNPACK_ALIGNMENT = int(z'0CF5', c_int)
   integer(c_int), parameter :: GL_BLEND = int(z'0BE2', c_int)
   integer(c_int), parameter :: GL_MULTISAMPLE = int(z'809D', c_int)
   integer(c_int), parameter :: GL_FRAMEBUFFER = int(z'8D40', c_int)
   integer(c_int), parameter :: GL_READ_FRAMEBUFFER = int(z'8CA8', c_int)
   integer(c_int), parameter :: GL_DRAW_FRAMEBUFFER = int(z'8CA9', c_int)
   integer(c_int), parameter :: GL_RENDERBUFFER = int(z'8D41', c_int)
   integer(c_int), parameter :: GL_COLOR_ATTACHMENT0 = int(z'8CE0', c_int)
   integer(c_int), parameter :: GL_FRAMEBUFFER_COMPLETE = int(z'8CD5', c_int)
   integer(c_int), parameter :: GL_RGBA8 = int(z'8058', c_int)
   integer(c_int), parameter :: GL_SAMPLES = int(z'80A9', c_int)
   integer(c_int), parameter :: GL_VENDOR_S = int(z'1F00', c_int)
   integer(c_int), parameter :: GL_RENDERER_S = int(z'1F01', c_int)
   integer(c_int), parameter :: GL_SRC_ALPHA = int(z'0302', c_int)
   integer(c_int), parameter :: GL_ONE_MINUS_SRC_ALPHA = int(z'0303', c_int)
   integer(c_int), parameter :: GL_ARRAY_BUFFER = int(z'8892', c_int)
   integer(c_int), parameter :: GL_DYNAMIC_DRAW = int(z'88E8', c_int)
   integer(c_int), parameter :: GL_VERTEX_SHADER = int(z'8B31', c_int)
   integer(c_int), parameter :: GL_FRAGMENT_SHADER = int(z'8B30', c_int)
   integer(c_int), parameter :: GL_COMPILE_STATUS = int(z'8B81', c_int)
   integer(c_int), parameter :: GL_LINK_STATUS = int(z'8B82', c_int)

   !--- entry point signatures ---------------------------------------
   abstract interface
      subroutine if_viewport(x, y, w, h)
         import :: c_int
         integer(c_int), value :: x, y, w, h
      end subroutine if_viewport
      subroutine if_clear_color(r, g, b, a)
         import :: c_float
         real(c_float), value :: r, g, b, a
      end subroutine if_clear_color
      subroutine if_clear(mask)
         import :: c_int
         integer(c_int), value :: mask
      end subroutine if_clear
      subroutine if_enable(cap)
         import :: c_int
         integer(c_int), value :: cap
      end subroutine if_enable
      subroutine if_blend(s, d)
         import :: c_int
         integer(c_int), value :: s, d
      end subroutine if_blend
      subroutine if_draw_arrays(m, f, c)
         import :: c_int
         integer(c_int), value :: m, f, c
      end subroutine if_draw_arrays
      subroutine if_gen_arrays(n, a)
         import :: c_int
         integer(c_int), value :: n
         integer(c_int) :: a
      end subroutine if_gen_arrays
      subroutine if_bind_array(a)
         import :: c_int
         integer(c_int), value :: a
      end subroutine if_bind_array
      subroutine if_bind_buffer(t, b)
         import :: c_int
         integer(c_int), value :: t, b
      end subroutine if_bind_buffer
      subroutine if_buffer_data(t, sz, data, usage)
         import :: c_int, c_ptr, c_size_t
         integer(c_int), value :: t
         integer(c_size_t), value :: sz
         type(c_ptr), value :: data
         integer(c_int), value :: usage
      end subroutine if_buffer_data
      subroutine if_enable_attrib(i)
         import :: c_int
         integer(c_int), value :: i
      end subroutine if_enable_attrib
      subroutine if_attrib_ptr(i, sz, typ, norm, stride, off)
         import :: c_int, c_size_t
         integer(c_int), value :: i, sz, typ, norm, stride
         integer(c_size_t), value :: off
      end subroutine if_attrib_ptr
      subroutine if_gen_textures(n, a)
         import :: c_int
         integer(c_int), value :: n
         integer(c_int) :: a
      end subroutine if_gen_textures
      subroutine if_bind_texture(t, x)
         import :: c_int
         integer(c_int), value :: t, x
      end subroutine if_bind_texture
      subroutine if_tex_param(t, p, v)
         import :: c_int
         integer(c_int), value :: t, p, v
      end subroutine if_tex_param
      subroutine if_tex_image(t, lvl, ifmt, w, h, b, fmt, typ, px)
         import :: c_int, c_ptr
         integer(c_int), value :: t, lvl, ifmt, w, h, b, fmt, typ
         type(c_ptr), value :: px
      end subroutine if_tex_image
      subroutine if_pixel_store(p, v)
         import :: c_int
         integer(c_int), value :: p, v
      end subroutine if_pixel_store
      subroutine if_active_texture(u)
         import :: c_int
         integer(c_int), value :: u
      end subroutine if_active_texture
      subroutine if_uniform1i(loc, v)
         import :: c_int
         integer(c_int), value :: loc, v
      end subroutine if_uniform1i
      function if_get_uniform(p, n) result(r)
         import :: c_char, c_int
         integer(c_int), value :: p
         character(kind=c_char) :: n(*)
         integer(c_int) :: r
      end function if_get_uniform
      subroutine if_use_program(p)
         import :: c_int
         integer(c_int), value :: p
      end subroutine if_use_program
      function if_create_shader(t) result(r)
         import :: c_int
         integer(c_int), value :: t
         integer(c_int) :: r
      end function if_create_shader
      subroutine if_shader_source(s, n, str, ln)
         import :: c_ptr, c_int
         integer(c_int), value :: s
         integer(c_int), value :: n
         type(c_ptr), value :: str, ln
      end subroutine if_shader_source
      subroutine if_compile_shader(s)
         import :: c_int
         integer(c_int), value :: s
      end subroutine if_compile_shader
      subroutine if_get_iv(s, p, v)
         import :: c_int
         integer(c_int), value :: s
         integer(c_int), value :: p
         integer(c_int) :: v
      end subroutine if_get_iv
      function if_create_program() result(r)
         import :: c_int
         integer(c_int) :: r
      end function if_create_program

      function if_get_error() result(r)
         import :: c_int
         integer(c_int) :: r
      end function if_get_error
      subroutine if_attach(p, s)
         import :: c_int
         integer(c_int), value :: p, s
      end subroutine if_attach
      subroutine if_link(p)
         import :: c_int
         integer(c_int), value :: p
      end subroutine if_link
      subroutine if_bind_fbo(t, f)
         import :: c_int
         integer(c_int), value :: t, f
      end subroutine if_bind_fbo
      subroutine if_bind_rbo(t, r)
         import :: c_int
         integer(c_int), value :: t, r
      end subroutine if_bind_rbo
      subroutine if_rbo_storage(t, samples, fmt, w, h)
         import :: c_int
         integer(c_int), value :: t, samples, fmt, w, h
      end subroutine if_rbo_storage
      subroutine if_fbo_rbo(t, att, rbt, rbo)
         import :: c_int
         integer(c_int), value :: t, att, rbt, rbo
      end subroutine if_fbo_rbo
      function if_check_fbo(t) result(r)
         import :: c_int
         integer(c_int), value :: t
         integer(c_int) :: r
      end function if_check_fbo
      subroutine if_get_log(s, n, buf)
         import :: c_int, c_char
         integer(c_int), value :: s, n
         character(kind=c_char) :: buf(*)
      end subroutine if_get_log
      function if_get_string(e) result(r)
         import :: c_int, c_ptr
         integer(c_int), value :: e
         type(c_ptr) :: r
      end function if_get_string
      subroutine if_fbo_tex(t, att, txt, txtr, lvl)
         import :: c_int
         integer(c_int), value :: t, att, txt, txtr, lvl
      end subroutine if_fbo_tex
      subroutine if_uniform1f(loc, v)
         import :: c_int, c_float
         integer(c_int), value :: loc
         real(c_float), value :: v
      end subroutine if_uniform1f
      subroutine if_uniform4f(loc, a, b, c, d)
         import :: c_int, c_float
         integer(c_int), value :: loc
         real(c_float), value :: a, b, c, d
      end subroutine if_uniform4f
      subroutine if_uniform2f(loc, a, b)
         import :: c_int, c_float
         integer(c_int), value :: loc
         real(c_float), value :: a, b
      end subroutine if_uniform2f
      subroutine if_blit(a0, a1, a2, a3, b0, b1, b2, b3, mask, filt)
         import :: c_int
         integer(c_int), value :: a0, a1, a2, a3, b0, b1, b2, b3, mask, filt
      end subroutine if_blit
   end interface

   !> Resolver signature: name -> function address.
   abstract interface
      function if_resolver(name) result(p)
         import :: c_ptr, c_char
         character(kind=c_char) :: name(*)
         type(c_ptr) :: p
      end function if_resolver
   end interface

   procedure(if_viewport), pointer, save :: p_viewport => null()
   procedure(if_clear_color), pointer, save :: p_clear_color => null()
   procedure(if_clear), pointer, save :: p_clear => null()
   procedure(if_enable), pointer, save :: p_enable => null()
   procedure(if_blend), pointer, save :: p_blend => null()
   procedure(if_draw_arrays), pointer, save :: p_draw => null()
   procedure(if_gen_arrays), pointer, save :: p_gen_vao => null()
   procedure(if_bind_array), pointer, save :: p_bind_vao => null()
   procedure(if_gen_arrays), pointer, save :: p_gen_buf => null()
   procedure(if_bind_buffer), pointer, save :: p_bind_buf => null()
   procedure(if_buffer_data), pointer, save :: p_buf_data => null()
   procedure(if_enable_attrib), pointer, save :: p_enable_attrib => null()
   procedure(if_attrib_ptr), pointer, save :: p_attrib_ptr => null()
   procedure(if_gen_textures), pointer, save :: p_gen_tex => null()
   procedure(if_bind_texture), pointer, save :: p_bind_tex => null()
   procedure(if_tex_param), pointer, save :: p_tex_param => null()
   procedure(if_tex_image), pointer, save :: p_tex_image => null()
   procedure(if_pixel_store), pointer, save :: p_pixel_store => null()
   procedure(if_active_texture), pointer, save :: p_active_tex => null()
   procedure(if_uniform1i), pointer, save :: p_uniform1i => null()
   procedure(if_get_uniform), pointer, save :: p_get_uniform => null()
   procedure(if_use_program), pointer, save :: p_use => null()
   procedure(if_create_shader), pointer, save :: p_create_shader => null()
   procedure(if_shader_source), pointer, save :: p_shader_source => null()
   procedure(if_compile_shader), pointer, save :: p_compile => null()
   procedure(if_get_iv), pointer, save :: p_shader_iv => null()
   procedure(if_create_program), pointer, save :: p_create_prog => null()
   procedure(if_attach), pointer, save :: p_attach => null()
   procedure(if_link), pointer, save :: p_link => null()
   procedure(if_get_iv), pointer, save :: p_prog_iv => null()
   procedure(if_get_error), pointer, save :: p_get_error => null()

   ! offscreen multisampled target, used to get real antialiasing
   procedure(if_gen_arrays), pointer, save :: p_gen_fbo => null()
   procedure(if_gen_arrays), pointer, save :: p_gen_rbo => null()
   procedure(if_bind_fbo), pointer, save :: p_bind_fbo => null()
   procedure(if_bind_rbo), pointer, save :: p_bind_rbo => null()
   procedure(if_rbo_storage), pointer, save :: p_rbo_storage => null()
   procedure(if_fbo_rbo), pointer, save :: p_fbo_rbo => null()
   procedure(if_check_fbo), pointer, save :: p_check_fbo => null()
   procedure(if_blit), pointer, save :: p_blit => null()
   procedure(if_fbo_tex), pointer, save :: p_attach_tex => null()
   procedure(if_uniform1f), pointer, save :: p_uniform1f => null()
   procedure(if_uniform2f), pointer, save :: p_uniform2f => null()
   procedure(if_uniform4f), pointer, save :: p_uniform4f => null()
   procedure(if_get_string), pointer, save :: p_get_string => null()
   procedure(if_get_log), pointer, save :: p_get_log => null()
   !> Scene copy used by glass: full res texture + single sample target.
   integer(c_int), save :: g_scene_fbo = 0, g_scene_tex = 0
   integer, save :: g_scene_w = 0, g_scene_h = 0

   integer(c_int), save, target :: g_prog = 0
   integer(c_int), save, target :: g_vao = 0
   integer(c_int), save, target :: g_vbo = 0
   integer(c_int), save, target :: g_tex = 0
   integer, save :: g_u_tex = -1, g_u_bg = -1, g_u_screen = -1
   integer, save :: g_u_glass = -1, g_u_glass_r = -1, g_u_time = -1
   logical, save :: g_ready = .false.
   logical, save :: g_first = .true.
   !> Multisample framebuffer. 0 means "draw straight to the window".
   integer(c_int), save :: g_fbo = 0
   integer(c_int), save :: g_rbo = 0
   integer, save :: g_fw = 0
   integer, save :: g_fh = 0
   integer, save :: g_samples = 0
   !> Supersample factor actually in use (1 when MSAA is available).
   integer, save :: g_ss = 1
   !> Backdrop (wallpaper + blur) state.
   integer(c_int), save :: g_wallp = 0, g_blurp = 0
   integer, save :: g_u_wall_time = -1, g_u_wall_res = -1
   integer, save :: g_u_blur_tex = -1, g_u_blur_dir = -1
   integer(c_int), save :: g_bg_fbo = 0, g_bg_fbo2 = 0
   integer(c_int), save :: g_bg_tex = 0, g_bg_tmp = 0
   integer, save :: g_bgw = 0, g_bgh = 0
   real(c_float), save :: g_time = 0.0_c_float

   type, extends(ui_gfx_backend) :: ui_gl_backend
      logical :: dummy = .false.
   contains
      procedure :: begin => gl_begin
      procedure :: draw => gl_draw
      procedure :: present => gl_present
      procedure :: shutdown => gl_shutdown
      procedure :: name => gl_name
      procedure :: resolve => gl_resolve
      procedure :: capture => gl_capture
   end type ui_gl_backend

   character(len=*), parameter :: VS_SRC = &
      '#version 330 core'//char(10)// &
      'layout(location=0) in vec2 aPos;'//char(10)// &
      'layout(location=1) in vec2 aUV;'//char(10)// &
      'layout(location=2) in vec4 aCol;'//char(10)// &
      'layout(location=3) in float aMode;'//char(10)// &
      'layout(location=4) in vec2 aLocal;'//char(10)// &
      'layout(location=5) in vec3 aShape;'//char(10)// &
      'out vec2 vUV; out vec4 vCol; out float vMode;'//char(10)// &
      'out vec2 vLoc; out vec3 vShape;'//char(10)// &
      'void main(){ vUV=aUV; vCol=aCol; vMode=aMode; vLoc=aLocal; vShape=aShape;'//char(10)// &
      '  gl_Position=vec4(aPos,0.0,1.0); }'//char(10)

   character(len=*), parameter :: FS_SRC = &
      '#version 330 core'//char(10)// &
      'in vec2 vUV; in vec4 vCol; in float vMode;'//char(10)// &
      'in vec2 vLoc; in vec3 vShape;'//char(10)// &
      'uniform sampler2D uTex;'//char(10)// &
      'uniform sampler2D uBg;'//char(10)// &
      'uniform vec2 uScreen;'//char(10)// &
      'uniform vec4 uGlass;'//char(10)// &
      'uniform float uGlassR;'//char(10)// &
      'uniform float uTime;'//char(10)// &
      'out vec4 oCol;'//char(10)// &
      'float hash(vec2 p){ return fract(sin(dot(p,vec2(41.3,289.1)))*43758.5453); }'//char(10)// &
      'float vnoise(vec2 p){ vec2 i=floor(p), f=fract(p); f=f*f*(3.0-2.0*f);'//char(10)// &
      '  return mix(mix(hash(i),hash(i+vec2(1,0)),f.x),'//char(10)// &
      '             mix(hash(i+vec2(0,1)),hash(i+vec2(1,1)),f.x), f.y); }'//char(10)// &
      'void main(){'//char(10)// &
      '  if (vMode < 0.5) {'//char(10)// &
      '    float a = texture(uTex,vUV).r;'//char(10)// &
      '    oCol = vec4(vCol.rgb, vCol.a*a);'//char(10)// &
      '    return;'//char(10)// &
      '  }'//char(10)// &
      '  vec2 uv = gl_FragCoord.xy/uScreen;'//char(10)// &
      '  if (vMode > 1.5) { oCol = vec4(texture(uBg,uv).rgb,1.0); return; }'//char(10)// &
      '  // DkoBot/ImGui-Gaussian-Blur model: the blurred scene IS the glass.'//char(10)// &
      '  // Draw it as a rounded image with a tint, alpha = how much tint.'//char(10)// &
      '  vec3 s = texture(uBg, uv).rgb;'//char(10)// &
      '  oCol = vec4(s * vCol.rgb, vCol.a);'//char(10)// &
      '}'//char(10)

   !> Pass-through vertex shader for full screen passes.
   character(len=*), parameter :: QUAD_VS = &
      '#version 330 core'//char(10)// &
      'layout(location=0) in vec2 aPos;'//char(10)// &
      'layout(location=1) in vec2 aUV;'//char(10)// &
      'out vec2 vUV;'//char(10)// &
      'void main(){ vUV=aUV; gl_Position=vec4(aPos,0.0,1.0); }'//char(10)

   !> Procedural wallpaper: domain warped fbm, slowly drifting.
   character(len=*), parameter :: WALL_FS = &
      '#version 330 core'//char(10)// &
      'in vec2 vUV; out vec4 oCol;'//char(10)// &
      'uniform float uTime; uniform vec2 uRes;'//char(10)// &
      'float h(vec2 p){ return fract(sin(dot(p,vec2(127.1,311.7)))*43758.5453); }'//char(10)// &
      'float n(vec2 p){ vec2 i=floor(p), f=fract(p); f=f*f*(3.0-2.0*f);'//char(10)// &
      '  return mix(mix(h(i),h(i+vec2(1,0)),f.x),'//char(10)// &
      '             mix(h(i+vec2(0,1)),h(i+vec2(1,1)),f.x), f.y); }'//char(10)// &
      'float fbm(vec2 p){ float s=0.0, a=0.5;'//char(10)// &
      '  for(int i=0;i<5;i++){ s+=a*n(p); p*=2.03; a*=0.5; } return s; }'//char(10)// &
      'void main(){'//char(10)// &
      '  vec2 p = vUV * (uRes.x/uRes.y) * 2.4;'//char(10)// &
      '  float t = uTime*0.10;'//char(10)// &
      '  float f = fbm(p + vec2(t,-0.7*t) + fbm(p*1.7 - 0.5*t));'//char(10)// &
      '  vec3 a = vec3(0.13,0.17,0.36);'//char(10)// &
      '  vec3 b = vec3(0.58,0.26,0.52);'//char(10)// &
      '  vec3 c = vec3(0.10,0.58,0.62);'//char(10)// &
      '  vec3 col = mix(a,b, smoothstep(0.22,0.86,f));'//char(10)// &
      '  col = mix(col, c, smoothstep(0.55,1.0, fbm(p*0.7 + vec2(-0.8*t,t))));'//char(10)// &
      '  col *= 0.72 + 0.38*vUV.y;'//char(10)// &
      '}'//char(10)

   !> One separable Gaussian pass (9 taps, sigma ~ 2.2 texels).
   character(len=*), parameter :: BLUR_FS = &
      '#version 330 core'//char(10)// &
      'in vec2 vUV; out vec4 oCol;'//char(10)// &
      'uniform sampler2D uTex; uniform vec2 uDir;'//char(10)// &
      'void main(){'//char(10)// &
      '  float w0=0.2270270270, w1=0.1945945946, w2=0.1216216216;'//char(10)// &
      '  float w3=0.0540540541, w4=0.0162162162;'//char(10)// &
      '  vec3 s = texture(uTex,vUV).rgb * w0;'//char(10)// &
      '  s += (texture(uTex,vUV+uDir*1.0).rgb + texture(uTex,vUV-uDir*1.0).rgb)*w1;'//char(10)// &
      '  s += (texture(uTex,vUV+uDir*2.0).rgb + texture(uTex,vUV-uDir*2.0).rgb)*w2;'//char(10)// &
      '  s += (texture(uTex,vUV+uDir*3.0).rgb + texture(uTex,vUV-uDir*3.0).rgb)*w3;'//char(10)// &
      '  s += (texture(uTex,vUV+uDir*4.0).rgb + texture(uTex,vUV-uDir*4.0).rgb)*w4;'//char(10)// &
      '  oCol = vec4(s,1.0);'//char(10)// &
      '}'//char(10)

contains

   logical function gl_null() result(r)

      r = .not. g_ready
   end function gl_null

   !> Print the driver's compile log when a shader does not build.
   subroutine check_shader(sh, tag)
      integer(c_int), value :: sh
      character(*), intent(in) :: tag
      integer(c_int) :: st
      character(kind=c_char) :: buf(4096)
      integer :: k
      character(len=4096) :: s

      call p_shader_iv(sh, GL_COMPILE_STATUS, st)
      if (st /= 0) return
      s = ' '
      if (associated(p_get_log)) then
         call p_get_log(sh, 4095_c_int, buf)
         do k = 1, 4095
            if (buf(k) == c_null_char) exit
            s(k:k) = buf(k)
         end do
      end if
      write (*, '(a,a,a)') 'shader failed: ', tag, trim(s)
   end subroutine check_shader

   !> Resolve every entry point and build GL objects.
   logical function gl_init(resolver) result(ok)
      procedure(if_resolver) :: resolver
      integer(c_int) :: vs, fs
      integer(c_int) :: status
      integer(ip) :: atlas(UI_ATLAS_W, UI_ATLAS_H)
      character(kind=c_char), allocatable, target :: src(:), bytes(:)
      type(c_ptr), target :: srcp(1)
      integer :: i, k

      ok = .false.

      call c_f_procpointer(fptr(resolver, 'glViewport'), p_viewport)
      call c_f_procpointer(fptr(resolver, 'glClearColor'), p_clear_color)
      call c_f_procpointer(fptr(resolver, 'glClear'), p_clear)
      call c_f_procpointer(fptr(resolver, 'glEnable'), p_enable)
      call c_f_procpointer(fptr(resolver, 'glBlendFunc'), p_blend)
      call c_f_procpointer(fptr(resolver, 'glDrawArrays'), p_draw)
      call c_f_procpointer(fptr(resolver, 'glGenVertexArrays'), p_gen_vao)
      call c_f_procpointer(fptr(resolver, 'glBindVertexArray'), p_bind_vao)
      call c_f_procpointer(fptr(resolver, 'glGenBuffers'), p_gen_buf)
      call c_f_procpointer(fptr(resolver, 'glBindBuffer'), p_bind_buf)
      call c_f_procpointer(fptr(resolver, 'glBufferData'), p_buf_data)
      call c_f_procpointer(fptr(resolver, 'glEnableVertexAttribArray'), p_enable_attrib)
      call c_f_procpointer(fptr(resolver, 'glVertexAttribPointer'), p_attrib_ptr)
      call c_f_procpointer(fptr(resolver, 'glGenTextures'), p_gen_tex)
      call c_f_procpointer(fptr(resolver, 'glBindTexture'), p_bind_tex)
      call c_f_procpointer(fptr(resolver, 'glTexParameteri'), p_tex_param)
      call c_f_procpointer(fptr(resolver, 'glTexImage2D'), p_tex_image)
      call c_f_procpointer(fptr(resolver, 'glPixelStorei'), p_pixel_store)
      call c_f_procpointer(fptr(resolver, 'glActiveTexture'), p_active_tex)
      call c_f_procpointer(fptr(resolver, 'glUniform1i'), p_uniform1i)
      call c_f_procpointer(fptr(resolver, 'glGetUniformLocation'), p_get_uniform)
      call c_f_procpointer(fptr(resolver, 'glUseProgram'), p_use)
      call c_f_procpointer(fptr(resolver, 'glCreateShader'), p_create_shader)
      call c_f_procpointer(fptr(resolver, 'glShaderSource'), p_shader_source)
      call c_f_procpointer(fptr(resolver, 'glCompileShader'), p_compile)
      call c_f_procpointer(fptr(resolver, 'glGetShaderiv'), p_shader_iv)
      call c_f_procpointer(fptr(resolver, 'glCreateProgram'), p_create_prog)
      call c_f_procpointer(fptr(resolver, 'glAttachShader'), p_attach)
      call c_f_procpointer(fptr(resolver, 'glLinkProgram'), p_link)
      call c_f_procpointer(fptr(resolver, 'glGetProgramiv'), p_prog_iv)
      call c_f_procpointer(fptr(resolver, 'glGetError'), p_get_error)
      call c_f_procpointer(fptr(resolver, 'glGenFramebuffers'), p_gen_fbo)
      call c_f_procpointer(fptr(resolver, 'glGenRenderbuffers'), p_gen_rbo)
      call c_f_procpointer(fptr(resolver, 'glBindFramebuffer'), p_bind_fbo)
      call c_f_procpointer(fptr(resolver, 'glBindRenderbuffer'), p_bind_rbo)
      call c_f_procpointer(fptr(resolver, 'glRenderbufferStorageMultisample'), &
                           p_rbo_storage)
      call c_f_procpointer(fptr(resolver, 'glFramebufferRenderbuffer'), p_fbo_rbo)
      call c_f_procpointer(fptr(resolver, 'glCheckFramebufferStatus'), p_check_fbo)
      call c_f_procpointer(fptr(resolver, 'glBlitFramebuffer'), p_blit)
      call c_f_procpointer(fptr(resolver, 'glFramebufferTexture2D'), p_attach_tex)
      call c_f_procpointer(fptr(resolver, 'glUniform1f'), p_uniform1f)
      call c_f_procpointer(fptr(resolver, 'glUniform2f'), p_uniform2f)
      call c_f_procpointer(fptr(resolver, 'glUniform4f'), p_uniform4f)
      call c_f_procpointer(fptr(resolver, 'glGetString'), p_get_string)
      call c_f_procpointer(fptr(resolver, 'glGetShaderInfoLog'), p_get_log)
      call c_f_procpointer(fptr(resolver, 'glGetError'), p_get_error)

      if (.not. associated(p_viewport)) return

      vs = p_create_shader(GL_VERTEX_SHADER)
      call shader_src(p_shader_source, vs, VS_SRC)
      call p_compile(vs)
      call p_shader_iv(vs, GL_COMPILE_STATUS, status)
      if (status == 0) return

      fs = p_create_shader(GL_FRAGMENT_SHADER)
      call shader_src(p_shader_source, fs, FS_SRC)
      call p_compile(fs)
      call p_shader_iv(fs, GL_COMPILE_STATUS, status)
      if (status == 0) return

      g_prog = p_create_prog()
      call p_attach(g_prog, vs)
      call p_attach(g_prog, fs)
      call p_link(g_prog)
      call p_prog_iv(g_prog, GL_LINK_STATUS, status)
      if (status == 0) return

      call p_gen_vao(1_c_int, g_vao)
      call p_bind_vao(g_vao)
      call p_gen_buf(1_c_int, g_vbo)
      call p_bind_buf(GL_ARRAY_BUFFER, g_vbo)
      call p_enable_attrib(0_c_int)
      call p_attrib_ptr(0_c_int, 2_c_int, GL_FLOAT, 0_c_int, &
                        int(ui_vertex_bytes(), c_int), 0_c_size_t)
      call p_enable_attrib(1_c_int)
      call p_attrib_ptr(1_c_int, 2_c_int, GL_FLOAT, 0_c_int, &
                        int(ui_vertex_bytes(), c_int), 8_c_size_t)
      call p_enable_attrib(2_c_int)
      call p_attrib_ptr(2_c_int, 4_c_int, GL_FLOAT, 0_c_int, &
                        int(ui_vertex_bytes(), c_int), 16_c_size_t)
      call p_enable_attrib(3_c_int)
      call p_attrib_ptr(3_c_int, 1_c_int, GL_FLOAT, 0_c_int, &
                        int(ui_vertex_bytes(), c_int), 32_c_size_t)
      call p_enable_attrib(4_c_int)
      call p_attrib_ptr(4_c_int, 2_c_int, GL_FLOAT, 0_c_int, &
                        int(ui_vertex_bytes(), c_int), 36_c_size_t)
      call p_enable_attrib(5_c_int)
      call p_attrib_ptr(5_c_int, 3_c_int, GL_FLOAT, 0_c_int, &
                        int(ui_vertex_bytes(), c_int), 44_c_size_t)

      call ui_atlas_build(atlas)
      allocate (bytes(UI_ATLAS_W*UI_ATLAS_H))
      k = 0
      do i = 1, UI_ATLAS_H
         do status = 1, UI_ATLAS_W
            k = k + 1
            bytes(k) = achar(iand(atlas(status, i), 255_ip))
         end do
      end do
      call p_pixel_store(GL_UNPACK_ALIGNMENT, 1_c_int)
      call p_gen_tex(1_c_int, g_tex)
      call p_active_tex(GL_TEXTURE0)
      call p_bind_tex(GL_TEXTURE_2D, g_tex)
      call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
      call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
      call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
      call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
      call p_tex_image(GL_TEXTURE_2D, 0_c_int, GL_R8, &
                       int(UI_ATLAS_W, c_int), int(UI_ATLAS_H, c_int), 0_c_int, &
                       GL_RED, GL_UNSIGNED_BYTE, c_loc(bytes))

      g_u_tex = int(p_get_uniform(g_prog, cstr('uTex')))
      g_u_bg = int(p_get_uniform(g_prog, cstr('uBg')))
      g_u_screen = int(p_get_uniform(g_prog, cstr('uScreen')))
      g_u_glass = int(p_get_uniform(g_prog, cstr('uGlass')))
      g_u_glass_r = int(p_get_uniform(g_prog, cstr('uGlassR')))
      g_u_time = int(p_get_uniform(g_prog, cstr('uTime')))
      call bg_init()
      g_ready = .true.
      ok = .true.
      write (*, '(a,i0,a,i0,a,i0,a,i0)') 'init done: prog=', g_prog, &
         ' vao=', g_vao, ' vbo=', g_vbo, ' err=', int(p_get_error())
      if (.false.) print *, k
   end function gl_init

   !> Wrapper so a procedure pointer can be passed as a dummy procedure.
   subroutine shader_src(proc_ptr, sh, text)
      procedure(if_shader_source) :: proc_ptr
      integer(c_int), value :: sh
      character(*), intent(in) :: text
      character(kind=c_char), allocatable, target :: buf(:)
      type(c_ptr), target :: p(1)
      integer :: i

      allocate (buf(len(text) + 1))
      do i = 1, len(text)
         buf(i) = text(i:i)
      end do
      buf(len(text) + 1) = c_null_char
      p(1) = c_loc(buf)
      call proc_ptr(sh, 1_c_int, c_loc(p), c_null_ptr)
   end subroutine shader_src

   type(c_funptr) function fptr(resolver, name) result(f)
      procedure(if_resolver) :: resolver
      character(*), intent(in) :: name
      type(c_ptr) :: p

      p = resolver(cstr(name))
      f = transfer(p, f)
   end function fptr

   !> Sample count we ask for. 4x is the sweet spot for hard edged geometry.
   integer function wanted_samples() result(n)
      n = 4
   end function wanted_samples

   !> (Re)create the offscreen target when the size changes.
   subroutine ensure_target(w, h)
      integer, intent(in) :: w, h
      integer :: want

      if (.not. associated(p_gen_fbo)) return
      if (g_fbo /= 0 .and. g_fw == w .and. g_fh == h) return

      if (g_fbo /= 0) then
         call p_bind_fbo(GL_FRAMEBUFFER, 0_c_int)
      end if

      g_fw = w
      g_fh = h
      want = wanted_samples()
      g_samples = 0
      g_ss = 1

      if (g_fbo == 0) call p_gen_fbo(1_c_int, g_fbo)
      if (g_rbo == 0) call p_gen_rbo(1_c_int, g_rbo)

      call p_bind_fbo(GL_FRAMEBUFFER, g_fbo)
      call p_bind_rbo(GL_RENDERBUFFER, g_rbo)
      call p_rbo_storage(GL_RENDERBUFFER, int(want, c_int), GL_RGBA8, &
                         int(w, c_int), int(h, c_int))
      call p_fbo_rbo(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, &
                     GL_RENDERBUFFER, g_rbo)
      if (p_check_fbo(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE) then
         g_samples = want
      else
         ! No multisampling here: render 2x oversized and let the blit
         ! box-filter it down. Slower, but the edges come out clean anywhere.
         g_samples = 0
         g_ss = 2
         call p_rbo_storage(GL_RENDERBUFFER, 0_c_int, GL_RGBA8, &
                            int(w*g_ss, c_int), int(h*g_ss, c_int))
         if (p_check_fbo(GL_FRAMEBUFFER) /= GL_FRAMEBUFFER_COMPLETE) then
            g_ss = 1
            call p_rbo_storage(GL_RENDERBUFFER, 0_c_int, GL_RGBA8, &
                               int(w, c_int), int(h, c_int))
         end if
      end if
      call p_bind_fbo(GL_FRAMEBUFFER, 0_c_int)
      write (*, '(a,i0,a,i0,a,i0,a,i0,a,i0)') 'gl: target ', g_fw, 'x', g_fh, &
         ' samples=', g_samples, ' ss=', g_ss, ' err=', int(p_get_error())
   end subroutine ensure_target

   subroutine gl_begin(self, width, height)
      class(ui_gl_backend), intent(inout) :: self
      integer, intent(in) :: width, height

      self%width = width
      self%height = height
      if (.not. g_ready) return
      call ensure_target(width, height)
      if (g_fbo /= 0) then
         call p_bind_fbo(GL_FRAMEBUFFER, g_fbo)
      end if
      call p_viewport(0_c_int, 0_c_int, int(width*g_ss, c_int), &
                      int(height*g_ss, c_int))
      call p_enable(GL_MULTISAMPLE)
      call p_enable(GL_BLEND)
      call p_blend(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      call p_clear_color(0.094_c_float, 0.102_c_float, 0.122_c_float, 1.0_c_float)
      call bg_render(width, height)
      call p_clear(GL_COLOR_BUFFER_BIT)
      ! The animated wallpaper is painted straight onto the frame. Sampling a
      ! texture for this was fragile (and blacked out completely when the
      ! capture path broke), so the background no longer depends on it.
      if (g_time < 0.02_c_float) then
         open (77, file='gl_dbg.txt', status='replace')
         write (77, '(a,i0,a,i0,a,i0,a,i0)') 'wallp=', g_wallp, ' blur=', g_blurp, &
            ' tex=', g_bg_tex, ' err=', int(p_get_error())
         close (77)
      end if
      if (g_wallp /= 0) then
         call p_use(g_wallp)
         g_time = g_time + 0.016_c_float
         if (g_u_wall_time >= 0) call p_uniform1f(int(g_u_wall_time, c_int), g_time)
         if (g_u_wall_res >= 0) call p_uniform2f(int(g_u_wall_res, c_int), &
                                                 real(width*g_ss, c_float), &
                                                 real(height*g_ss, c_float))
         call draw_quad(g_wallp)
      end if
   end subroutine gl_begin

   subroutine gl_draw(self, batch)
      class(ui_gl_backend), intent(inout) :: self
      type(ui_batch), intent(in), target :: batch
      integer(c_size_t) :: nbytes

      if (.not. g_ready) return
      if (batch%count < 3) return

      nbytes = int(batch%count, c_size_t)*int(ui_vertex_bytes(), c_size_t)
      call p_bind_vao(g_vao)
      call p_bind_buf(GL_ARRAY_BUFFER, g_vbo)
      call p_buf_data(GL_ARRAY_BUFFER, nbytes, c_loc(batch%v(1)), GL_DYNAMIC_DRAW)

      call p_use(g_prog)
      call p_active_tex(GL_TEXTURE0)
      call p_bind_tex(GL_TEXTURE_2D, g_tex)
      if (g_u_tex >= 0) call p_uniform1i(int(g_u_tex, c_int), 0_c_int)
      call p_active_tex(GL_TEXTURE1)
      call p_bind_tex(GL_TEXTURE_2D, g_bg_tex)
      call p_active_tex(GL_TEXTURE0)
      if (g_u_bg >= 0) call p_uniform1i(int(g_u_bg, c_int), 1_c_int)
      if (g_u_screen >= 0) call p_uniform2f(int(g_u_screen, c_int), &
         real(self%width*g_ss, c_float), real(self%height*g_ss, c_float))
      if (g_u_glass >= 0) then
         call p_uniform4f(int(g_u_glass, c_int), &
            real(batch%glass_rect(1)*g_ss, c_float), &
            real(batch%glass_rect(2)*g_ss, c_float), &
            real(batch%glass_rect(3)*g_ss, c_float), &
            real(batch%glass_rect(4)*g_ss, c_float))
      end if
      if (g_u_time >= 0) call p_uniform1f(int(g_u_time, c_int), g_time)
      if (g_u_glass_r >= 0) call p_uniform1f(int(g_u_glass_r, c_int), &
                                              real(batch%glass_r*g_ss, c_float))

      call p_draw(GL_TRIANGLES, 0_c_int, int(batch%count, c_int))
      if (g_first) then
         g_first = .false.
         write (*, '(a,i0,a,i0,a,i0,a,i0)') 'gl: drew ', batch%count, ' verts, err=', &
            int(p_get_error()), ' prog=', g_prog, ' msaa=', g_samples
      end if
      if (.false.) print *, self%width
   end subroutine gl_draw

   subroutine gl_capture(self, width, height)
      class(ui_gl_backend), intent(inout) :: self
      integer, intent(in) :: width, height
      integer :: i

      if (.not. g_ready) return
      if (.not. associated(p_attach_tex)) return

      ! 1. give the scene its own colour texture at full resolution
      if (g_scene_tex == 0) then
         call p_gen_tex(1_c_int, g_scene_tex)
         call p_gen_fbo(1_c_int, g_scene_fbo)
      end if
      if (g_scene_w /= width .or. g_scene_h /= height) then
         g_scene_w = width
         g_scene_h = height
         call p_bind_tex(GL_TEXTURE_2D, g_scene_tex)
         call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_F)
         call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_F)
         call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
         call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
         call p_tex_image(GL_TEXTURE_2D, 0_c_int, GL_RGBA8, &
                          int(width, c_int), int(height, c_int), 0_c_int, &
                          GL_RGBA_C, GL_UNSIGNED_BYTE, c_null_ptr)
         call p_bind_tex(GL_TEXTURE_2D, 0_c_int)
         call p_bind_fbo(GL_FRAMEBUFFER, g_scene_fbo)
         call p_attach_tex(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, &
                           g_scene_tex, 0_c_int)
      end if

      ! 2. resolve the multisampled frame into it
      call p_bind_fbo(GL_READ_FRAMEBUFFER, g_fbo)
      call p_bind_fbo(GL_DRAW_FRAMEBUFFER, g_scene_fbo)
      call p_blit(0_c_int, 0_c_int, int(width, c_int), int(height, c_int), &
                  0_c_int, 0_c_int, int(width, c_int), int(height, c_int), &
                  GL_COLOR_BUFFER_BIT, GL_NEAREST_F)

      ! 3. blur it: H into g_bg_tmp, V back into g_bg_tex (what glass samples)
      if (g_blurp /= 0 .and. g_bgw > 0) then
         call p_bind_fbo(GL_FRAMEBUFFER, g_bg_fbo2)
         call p_attach_tex(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, &
                           g_bg_tmp, 0_c_int)
         call p_viewport(0_c_int, 0_c_int, int(g_bgw, c_int), int(g_bgh, c_int))
         call p_use(g_blurp)
         call p_active_tex(GL_TEXTURE0)
         call p_bind_tex(GL_TEXTURE_2D, g_scene_tex)
         if (g_u_blur_tex >= 0) call p_uniform1i(int(g_u_blur_tex, c_int), 0_c_int)
         if (g_u_blur_dir >= 0) call p_uniform2f(int(g_u_blur_dir, c_int), &
                                                 1.4_c_float/real(g_bgw, c_float), 0.0_c_float)
         call draw_quad(g_blurp)

         call p_bind_fbo(GL_FRAMEBUFFER, g_bg_fbo)
         call p_attach_tex(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, &
                           g_bg_tex, 0_c_int)
         call p_bind_tex(GL_TEXTURE_2D, g_bg_tmp)
         if (g_u_blur_dir >= 0) call p_uniform2f(int(g_u_blur_dir, c_int), &
                                                 0.0_c_float, 1.4_c_float/real(g_bgh, c_float))
         call draw_quad(g_blurp)
      end if

      ! 4. back to the frame we were drawing into
      if (g_fbo /= 0) then
         call p_bind_fbo(GL_FRAMEBUFFER, g_fbo)
      else
         call p_bind_fbo(GL_FRAMEBUFFER, 0_c_int)
      end if
      call p_viewport(0_c_int, 0_c_int, int(width*g_ss, c_int), int(height*g_ss, c_int))
      if (.false.) print *, i
   end subroutine gl_capture
   !> Resolve the offscreen target onto the window and unbind it.
   subroutine gl_resolve(self)
      class(ui_gl_backend), intent(inout) :: self
      integer(c_int) :: w, h, fw, fh, filt

      if (.not. g_ready) return
      if (g_fbo == 0) return

      w = int(self%width, c_int)
      h = int(self%height, c_int)
      fw = int(self%width*g_ss, c_int)
      fh = int(self%height*g_ss, c_int)
      filt = GL_NEAREST_F
      if (g_ss > 1) filt = GL_LINEAR_F
      call p_bind_fbo(GL_READ_FRAMEBUFFER, g_fbo)
      call p_bind_fbo(GL_DRAW_FRAMEBUFFER, 0_c_int)
      call p_blit(0_c_int, 0_c_int, fw, fh, 0_c_int, 0_c_int, w, h, &
                  GL_COLOR_BUFFER_BIT, filt)
      call p_bind_fbo(GL_FRAMEBUFFER, 0_c_int)
   end subroutine gl_resolve

   subroutine gl_present(self)
      class(ui_gl_backend), intent(inout) :: self

      if (.false.) print *, self%width
   end subroutine gl_present

   subroutine gl_shutdown(self)
      class(ui_gl_backend), intent(inout) :: self

      g_ready = .false.
      if (.false.) print *, self%width
   end subroutine gl_shutdown

   function gl_name(self) result(s)
      class(ui_gl_backend), intent(in) :: self
      character(len=:), allocatable :: s

      s = 'opengl'
      if (.false.) print *, self%width
   end function gl_name

   pure function cstr(s) result(c)
      character(*), intent(in) :: s
      character(len=len_trim(s) + 1, kind=c_char) :: c
      integer :: i

      do i = 1, len_trim(s)
         c(i:i) = s(i:i)
      end do
      c(len_trim(s) + 1:len_trim(s) + 1) = c_null_char
   end function cstr

   !==================================================================
   ! Dynamic backdrop: procedural wallpaper + separable Gaussian blur.
   ! This is what the frosted / Mica panels sample.
   !==================================================================

   !> Build the two helper programs. Called once from gl_init.
   subroutine bg_init()
      integer(c_int) :: vs, fs

      vs = p_create_shader(GL_VERTEX_SHADER)
      call shader_src(p_shader_source, vs, QUAD_VS)
      call p_compile(vs)
      fs = p_create_shader(GL_FRAGMENT_SHADER)
      call shader_src(p_shader_source, fs, WALL_FS)
      call p_compile(fs)
      g_wallp = p_create_prog()
      call p_attach(g_wallp, vs)
      call p_attach(g_wallp, fs)
      call p_link(g_wallp)

      fs = p_create_shader(GL_FRAGMENT_SHADER)
      call shader_src(p_shader_source, fs, BLUR_FS)
      call p_compile(fs)
      g_blurp = p_create_prog()
      call p_attach(g_blurp, vs)
      call p_attach(g_blurp, fs)
      call p_link(g_blurp)

      g_u_wall_time = int(p_get_uniform(g_wallp, cstr('uTime')))
      g_u_wall_res = int(p_get_uniform(g_wallp, cstr('uRes')))
      g_u_blur_tex = int(p_get_uniform(g_blurp, cstr('uTex')))
      g_u_blur_dir = int(p_get_uniform(g_blurp, cstr('uDir')))
   end subroutine bg_init

   !> (Re)allocate the backdrop targets for the current window size.
   subroutine bg_alloc(w, h)
      integer, intent(in) :: w, h
      integer :: bw, bh, i

      bw = max(w/2, 16)
      bh = max(h/2, 16)
      if (g_bg_tex /= 0 .and. g_bgw == bw .and. g_bgh == bh) return

      g_bgw = bw
      g_bgh = bh
      if (g_bg_tex == 0) then
         call p_gen_tex(1_c_int, g_bg_tex)
         call p_gen_tex(1_c_int, g_bg_tmp)
         call p_gen_fbo(1_c_int, g_bg_fbo)
         call p_gen_fbo(1_c_int, g_bg_fbo2)
      end if
      do i = 1, 2
         if (i == 1) then
            call p_bind_tex(GL_TEXTURE_2D, g_bg_tex)
         else
            call p_bind_tex(GL_TEXTURE_2D, g_bg_tmp)
         end if
         call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_F)
         call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_F)
         call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
         call p_tex_param(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
         call p_tex_image(GL_TEXTURE_2D, 0_c_int, GL_RGBA8, &
                          int(bw, c_int), int(bh, c_int), 0_c_int, &
                          GL_RGBA_C, GL_UNSIGNED_BYTE, c_null_ptr)
      end do
      call p_bind_tex(GL_TEXTURE_2D, 0_c_int)
   end subroutine bg_alloc

   !> Upload a full screen quad (NDC positions + 0..1 uv) and draw it.
   subroutine draw_quad(prog)
      integer(c_int), intent(in) :: prog
      type(ui_vertex), allocatable, target :: q(:)
      type(c_ptr) :: ptr

      allocate (q(6))
      q(1) = vtx(-1.0_wp, -1.0_wp, 0.0_wp, 0.0_wp)
      q(2) = vtx(1.0_wp, -1.0_wp, 1.0_wp, 0.0_wp)
      q(3) = vtx(1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp)
      q(4) = vtx(-1.0_wp, -1.0_wp, 0.0_wp, 0.0_wp)
      q(5) = vtx(1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp)
      q(6) = vtx(-1.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)

      call p_bind_vao(g_vao)
      call p_bind_buf(GL_ARRAY_BUFFER, g_vbo)
      ptr = c_loc(q(1))
      call p_buf_data(GL_ARRAY_BUFFER, &
                      int(6, c_size_t)*int(ui_vertex_bytes(), c_size_t), &
                      ptr, GL_DYNAMIC_DRAW)
      call p_use(prog)
      call p_draw(GL_TRIANGLES, 0_c_int, 6_c_int)
   end subroutine draw_quad

   pure function vtx(x, y, u, v) result(r)
      real(wp), intent(in) :: x, y, u, v
      type(ui_vertex) :: r

      r%x = real(x, c_float)
      r%y = real(y, c_float)
      r%u = real(u, c_float)
      r%v = real(v, c_float)
      r%r = 1.0_c_float
      r%g = 1.0_c_float
      r%b = 1.0_c_float
      r%a = 1.0_c_float
      r%mode = 0.0_c_float
   end function vtx

   !> Render the wallpaper, then blur it twice (horizontal + vertical).
   subroutine bg_render(w, h)
      integer, intent(in) :: w, h
      real(c_float) :: dx, dy
      integer :: iter

      if (.not. g_ready .or. g_wallp == 0) return
      if (.not. associated(p_gen_fbo)) return
      call bg_alloc(w, h)

      ! 1. animate the wallpaper into g_bg_tex
      call p_bind_fbo(GL_FRAMEBUFFER, g_bg_fbo)
      call p_attach_tex(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, g_bg_tex, 0_c_int)
      call p_viewport(0_c_int, 0_c_int, int(g_bgw, c_int), int(g_bgh, c_int))
      call p_clear(GL_COLOR_BUFFER_BIT)
      call p_use(g_wallp)
      g_time = g_time + 0.016_c_float
      if (g_u_wall_time >= 0) call p_uniform1f(int(g_u_wall_time, c_int), g_time)
      if (g_u_wall_res >= 0) call p_uniform2f(int(g_u_wall_res, c_int), &
                                              real(g_bgw, c_float), real(g_bgh, c_float))
      call draw_quad(g_wallp)

      ! 2/3. separable gaussian, run twice with a wider step. This is the
      ! trick from blur-my-shell: brute force the sigma by repeating the
      ! H/V pair instead of using a huge kernel in one pass.
      call p_use(g_blurp)
      call p_active_tex(GL_TEXTURE0)
      if (g_u_blur_tex >= 0) call p_uniform1i(int(g_u_blur_tex, c_int), 0_c_int)
      do iter = 1, 3
         call p_bind_fbo(GL_FRAMEBUFFER, g_bg_fbo2)
         call p_attach_tex(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, &
                           g_bg_tmp, 0_c_int)
         call p_bind_tex(GL_TEXTURE_2D, g_bg_tex)
         if (g_u_blur_dir >= 0) call p_uniform2f(int(g_u_blur_dir, c_int), &
                                                 2.4_c_float/real(g_bgw, c_float), 0.0_c_float)
         call draw_quad(g_blurp)

         call p_bind_fbo(GL_FRAMEBUFFER, g_bg_fbo)
         call p_attach_tex(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, &
                           g_bg_tex, 0_c_int)
         call p_bind_tex(GL_TEXTURE_2D, g_bg_tmp)
         if (g_u_blur_dir >= 0) call p_uniform2f(int(g_u_blur_dir, c_int), &
                                                 0.0_c_float, 2.4_c_float/real(g_bgh, c_float))
         call draw_quad(g_blurp)
      end do
      ! 4. back to the main target
      if (g_fbo /= 0) then
         call p_bind_fbo(GL_FRAMEBUFFER, g_fbo)
      else
         call p_bind_fbo(GL_FRAMEBUFFER, 0_c_int)
      end if
      call p_viewport(0_c_int, 0_c_int, int(w*g_ss, c_int), int(h*g_ss, c_int))
   end subroutine bg_render

end module ui_gfx_gl
