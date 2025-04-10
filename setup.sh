#!/bin/bash
#
# This script initializes the environment defined by & expected by the dotfiles
# in this repository. This includes copying dotfiles to their expected
# locations.

usage() {
    echo "usage: setup.sh [overwrite-existing-files: -f] [help: -h]"
    echo "-f (optional): overwrite existing configuration files"
    echo "-h: show help (this dialogue)"
}

#
# Parse arguments
#

OPT_OVERWRITE_EXISTING_FILES=false
while getopts "fh" opt; do
    case $opt in
        f) OPT_OVERWRITE_EXISTING_FILES=true ;;
        h) usage ; exit 0 ;;
        \?) usage ; exit 1 ;;
    esac
done

echo "Running with options:"
echo "OVERWRITE_EXISTING_FILES: $OPT_OVERWRITE_EXISTING_FILES"
echo "---------------------------------"

#
# Copy dotfiles
#

SRC_CONFIG_DIRS=("$(pwd)/bash" "$(pwd)/emacs" "$(pwd)/tmux")
DST_CONFIG_DIRS=("$HOME" "$HOME/.emacs.d" "$HOME")
length=${#SRC_CONFIG_DIRS[@]}
for ((i=0; i<$length; i++)); do
    src_config_dir=${SRC_CONFIG_DIRS[$i]}
    dst_config_dir=${DST_CONFIG_DIRS[$i]}
    if [ ! -d "$src_config_dir" ]; then
        echo "ERROR: $src_config_dir does not exist - skipping."
        continue
    fi
    if [ ! -d "$dst_config_dir" ]; then
        echo "ERROR: cannot copy $src_config_dir/* to $dst_config_dir because $dst_config_dir does not exist - skipping."
        continue
    fi

    for src_file in "$src_config_dir"/* "$src_config_dir"/.*; do
        base_file=$(basename "$src_file")
        # Skip hidden files that would never be intentionally symlinked
        if [[ "$base_file" == "." || "$base_file" == ".." || "$base_file" == "*" ]]; then
            continue
        fi

        # Set flags for 'ln' based on the arguments passed to this script
        ln_force_write_flag=""
        if [ "$OPT_OVERWRITE_EXISTING_FILES" = true ]; then
            ln_force_write_flag="f"
        fi

        dst_file="$dst_config_dir"/"$base_file"
        echo "Symlinking $src_file to $dst_file"
        ln -s"$ln_force_write_flag" "$src_file" "$dst_file"
    done
done
