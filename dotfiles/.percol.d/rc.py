# Run command file for percol
# X / _ / X
percol.view.PROMPT  = u"<bold><yellow>X / _ / X</yellow></bold> %q"

import percol.actions as actions

def finish_with_quote_output(percol):
    percol.actions = (actions.output_to_stdout_double_quote, )
    percol.finish()

# Emacs like
percol.import_keymap({
    "C-h" : lambda percol: percol.command.delete_backward_char(),
    "C-d" : lambda percol: percol.command.delete_forward_char(),
    "C-k" : lambda percol: percol.command.kill_end_of_line(),
    "C-y" : lambda percol: percol.command.yank(),
    "C-t" : lambda percol: percol.command.transpose_chars(),
    "C-a" : lambda percol: percol.command.beginning_of_line(),
    "C-e" : lambda percol: percol.command.end_of_line(),
    "C-b" : lambda percol: percol.command.backward_char(),
    "C-f" : lambda percol: percol.command.forward_char(),
    "M-f" : lambda percol: percol.command.forward_word(),
    "M-b" : lambda percol: percol.command.backward_word(),
    "M-d" : lambda percol: percol.command.delete_forward_word(),
    "M-<backspace>" : lambda percol: percol.command.delete_backward_word(),
    "C-n" : lambda percol: percol.command.select_next(),
    "C-p" : lambda percol: percol.command.select_previous(),
    "C-v" : lambda percol: percol.command.select_next_page(),
    "M-v" : lambda percol: percol.command.select_previous_page(),
    "M-<" : lambda percol: percol.command.select_top(),
    "M->" : lambda percol: percol.command.select_bottom(),
    "C-m" : lambda percol: percol.finish(),
    "C-j" : finish_with_quote_output,
    "C-g" : lambda percol: percol.cancel(),
    "M-t" : lambda percol: percol.command.toggle_mark_all(),
    "M-m" : lambda percol: percol.command.toggle_mark_and_next(),
})
