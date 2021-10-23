conky.config = {
    alignment = 'top_left',
    own_window_colour = '0C272D',
--    own_window_transparent = true,
    background = false,
    border_width = 2,
    cpu_avg_samples = 2,
    default_color = 'D6685E',
    default_outline_color = '7D4C4E',
    default_shade_color = 'B19B75',
    double_buffer = false,
    draw_borders = false,
    draw_graph_borders = false,
    draw_outline = false,
    draw_shades = true,
    extra_newline = false,
    font = 'DejaVu Sans Mono:size=12',
    minimum_width = 500,
    minimum_height = 5,
    gap_x = 4,
    gap_y = -4,
    net_avg_samples = 2,
    no_buffers = true,
    own_window_hints = 'undecorated,sticky,skip_taskbar,skip_pager',
    out_to_console = false,
    out_to_ncurses = false,
    out_to_stderr = false,
    out_to_x = true,
    own_window = true,
    own_window_type = 'desktop',
    show_graph_range = false,
    show_graph_scale = false,
    stippled_borders = 0,
    update_interval = 4.0,
    uppercase = false,
    use_spacer = 'none',
    use_xft = true,
}

conky.text = [[
$nodename $kernel $machine
$hr
${color B19B75}Uptime:$color $uptime
${color B19B75}RAM Usage:$color $mem/$memmax - $memperc%
${membar 8}
$hr
${color EAB0B1}File systems:
 / $color${fs_used /}/${fs_size /}
${fs_bar 6 /}
Networking:
${color EAB0B1}Up:$color ${upspeed} ${color EAB0B1} - Down:$color ${downspeed}

${color grey}Frequency (in MHz):$color $freq
${color grey}Frequency (in GHz):$color $freq_g
$hr
]]
