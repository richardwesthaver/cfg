conky.config = {
    alignment = 'middle_left',
    background = false,
    own_window_colour = '36313B',
    border_width = 2,
    cpu_avg_samples = 2,
    default_color = 'F9F2D3',
    default_outline_color = 'white',
    default_shade_color = '7D4C4E',
    double_buffer = false,
    draw_borders = false,
    draw_graph_borders = false,
    draw_outline = false,
    draw_shades = true,
    extra_newline = false,
    font = 'DejaVu Sans Mono:size=12',
    minimum_height = 5,
    gap_x = 4,
    gap_y = 64,
    own_window_hints = 'undecorated,sticky,skip_taskbar,skip_pager',
    minimum_width = 500,
    no_buffers = true,
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
${color grey}CPU Usage:$color $cpu%
${color pink}${cpubar 8}$color
$hr
${color grey}Processes:$color $processes  ${color grey}Running:$color $running_processes
$hr
Name              PID     CPU%   MEM%
${color lightgrey} ${top name 1} ${top pid 1} ${top cpu 1} ${top mem 1}
${color lightgrey} ${top name 2} ${top pid 2} ${top cpu 2} ${top mem 2}
${color lightgrey} ${top name 3} ${top pid 3} ${top cpu 3} ${top mem 3}
${color lightgrey} ${top name 4} ${top pid 4} ${top cpu 4} ${top mem 4}
${color lightgrey} ${top name 5} ${top pid 5} ${top cpu 5} ${top mem 5}
${color lightgrey} ${top name 6} ${top pid 6} ${top cpu 6} ${top mem 6}
${color lightgrey} ${top name 7} ${top pid 7} ${top cpu 7} ${top mem 7}
${color lightgrey} ${top name 8} ${top pid 8} ${top cpu 8} ${top mem 8}
$hr
]]
