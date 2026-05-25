
// Add holder for events
R3JS.Viewer.prototype.eventListeners.push({
    name: "points-selected",
    fn: function (e) {
        let viewer = e.detail.viewer;
        // viewer.graphics.ptStyles = viewer.graphics.plusSelected;
        viewer.updatePointStyles();
    }
});

R3JS.Viewer.prototype.eventListeners.push({
    name: "points-deselected",
    fn: function (e) {
        let viewer = e.detail.viewer;
        // viewer.graphics.ptStyles = viewer.graphics.noneSelected;
        viewer.updatePointStyles();
    }
});

// Set selection styles
// Racmacs.App.prototype.graphics = {

//   noneSelected : {
//     selected         : {
//         opacity : 1.0,
//         outlineColor : "#ff0066"
//     },
//     selected_hover   : {
//         opacity : 0.8
//     },
//     deselected       : {
//         // opacity : 0.6
//         opacity : 0.8
//     },
//     deselected_hover : {
//         opacity : 1.0
//     }
//   },

//   plusSelected : {
//     selected         : {
//         opacity : 1.0,
//         outlineColor : "#ff0066"
//     },
//     selected_hover   : {
//         opacity : 0.8
//     },
//     deselected       : {
//         opacity : 0.2
//     },
//     deselected_hover : {
//         opacity : 0.6
//     }
//   }

// };

// Racmacs.App.prototype.graphics.ptStyles = Racmacs.App.prototype.graphics.noneSelected;

// Set default point styles
Racmacs.App.prototype.setDefaultStyleset = function (dim) {

    this.styleset = {

        // Styles when some points are selected
        selections: {
            hovered: {
                selected: {
                    opacity: 1.0,
                    outlineColor: "#ff0066"
                },
                highlighted: {
                    opacity: 1.0
                },
                unselected: {
                    opacity: 0.8
                }
            },
            unhovered: {
                selected: {
                    opacity: 1.0,
                    outlineColor: "#ff0066"
                },
                highlighted: {
                    opacity: 1.0
                },
                unselected: {
                    opacity: 0.4
                }
            }
        },

        // Styles when no points are selected
        noselections: {
            hovered: {
                highlighted: {
                    opacity: 1.0
                },
                unselected: {
                    opacity: 1.0
                }
            },
            unhovered: {
                highlighted: {
                    opacity: 1.0
                },
                unselected: {
                    opacity: 0.9
                }
            }
        },

        // Styles when some points are hovered
        hoveredpoints: {
            hovered: {
                selected: {
                    opacity: 1.0,
                    outlineColor: "#ff0066"
                },
                highlighted: {
                    opacity: 1.0
                },
                unselected: {
                    opacity: 1.0
                }
            },
            unhovered: {
                selected: {
                    opacity: 1.0,
                    outlineColor: "#ff0066"
                },
                highlighted: {
                    opacity: 1.0
                },
                unselected: {
                    opacity: 0.6
                }
            }
        }

    };

    // Make any alterations based on dimensions provided
    if (dim !== undefined) {
        if (dim == 3) {
            this.styleset.noselections.unhovered.unselected.opacity = 0.8;
        }
    }

}

// Change background click event listener
Racmacs.App.prototype.clickBackground = function (e) {

    if (!e.shiftKey
        && !e.metaKey
        && !this.dragMode
        && !this.viewport.mouse.moved) {
        this.deselectAll();
    }

};

Racmacs.Point.prototype.click = function (e) {

    if (!this.viewer.dragMode) {
        if (!e.shiftKey && !e.metaKey) {
            this.viewer.deselectAll();
        }
        if (!this.viewer.viewport.mouse ||
            !this.viewer.viewport.mouse.moved) {
            if (!e.metaKey) {
                this.select();
            } else {
                if (this.selected) {
                    this.deselect();
                } else {
                    this.select();
                }
            }
        }
    }

}


// Set prototypes
Racmacs.Point.prototype.hover = function () {

    if (this.shown && !this.hovered && !this.viewer.selectRectMode) {

        this.hovered = true;

        // Add to list of hovered objects
        this.viewer.hovered_pts.push(this);
        this.viewer.points_hovered = true;

        // Show a fill for transparent objects
        if (this.fillColor == "transparent") {
            if (this.element) {
                this.element.setFillColor(this.outlineColor);
                this.element.setFillOpacity(0.3);
            }
        }

        this.viewer.updatePointStyles();

        // Show the point info
        this.moveToTop();
        this.showInfo();

        // Run additional hover functions
        this.viewer.dispatchEvent("point-hovered", {
            point: this
        });

    }

}

Racmacs.Point.prototype.dehover = function () {

    if (this.hovered) {

        this.hovered = false;

        // Hide the fill for transparent objects
        if (this.fillColor == "transparent") {
            if (this.element) {
                this.element.setFillColor("#ffffff");
                this.element.setFillOpacity(0);
            }
        }


        // Show the point info
        if (!this.selected) {
            this.moveFromTop();
        }
        this.hideInfo();

        // Remove from list of hovered objects
        var index = this.viewer.hovered_pts.indexOf(this);
        if (index > -1) {
            this.viewer.hovered_pts.splice(index, 1);
        }

        // Delay clearing of global hovered flag to avoid flicker when quickly moving
        // between points. Use a viewer-scoped timeout so multiple dehovers reset it.
        if (this.viewer.hovered_pts.length == 0) {
            var viewer = this.viewer;
            var delay = (viewer.hoverClearDelay !== undefined) ? viewer.hoverClearDelay : 50; // ms
            if (viewer._hoverClearTimeout) {
                clearTimeout(viewer._hoverClearTimeout);
            }
            viewer._hoverClearTimeout = setTimeout(function () {
                if (viewer.hovered_pts.length == 0) {
                    viewer.points_hovered = false;
                    viewer.updatePointStyles();
                }
                viewer._hoverClearTimeout = null;
            }, delay);
        } else {
            // still have hovered points; update immediately
            this.viewer.updatePointStyles();
        }

        // Run additional dehover functions
        this.viewer.dispatchEvent("point-dehovered", {
            point: this
        });

    }

}


// Change background click event listener
Racmacs.App.prototype.deselectAll = function (except = null) {

    let points_to_remove = this.selected_pts.slice();
    if (except !== null) {
        let index = points_to_remove.indexOf(except);
        if (index !== -1) points_to_remove.splice(index, 1)
    }
    points_to_remove.map(p => p.deselect());

}

// Add function to update styles of all points
Racmacs.App.prototype.updatePointStyles = function () {
    if (this.points) {
        for (var i = 0; i < this.points.length; i++) {
            this.points[i].updateDisplay();
        }
    }
}



Racmacs.Point.prototype.toggleSelection = function () {
    if (this.selected) {
        this.deselect();
    } else {
        this.select();
    }
}

Racmacs.Point.prototype.select = function (fire_handler = true) {

    if (this.shown && !this.selected) {

        var viewer = this.viewer;
        viewer.points_selected = true;

        // Toggle selection
        this.selected = true;

        // Update point style
        this.moveToTop();
        this.updateDisplay();

        // Add to list of selected objects
        viewer.selected_pts.push(this);

        // Run events relating to points being selected
        if (viewer.selected_pts.length == 1) {
            this.viewer.dispatchEvent("points-selected", {
                viewer: this.viewer
            });
        }

        // Run select functions
        this.viewer.dispatchEvent("point-selected", {
            point: this
        });

    }

    // Fire the on select handler
    if (fire_handler) {
        this.viewer.onSelectPoint();
    }

}

Racmacs.Point.prototype.deselect = function (fire_handler = true) {

    if (this.selected) {

        var viewer = this.viewer;

        // Toggle deselection
        this.selected = false;

        // Update the point style
        this.moveFromTop();
        this.updateDisplay();

        // Remove from list of selected objects
        var index = viewer.selected_pts.indexOf(this);
        if (index > -1) {
            viewer.selected_pts.splice(index, 1);
        }

        // Run deselect functions
        this.viewer.dispatchEvent("point-deselected", {
            point: this
        });

        // Run events relating to points being deselected
        if (viewer.selected_pts.length == 0) {
            viewer.points_selected = false;
            this.viewer.dispatchEvent("points-deselected", {
                viewer: this.viewer
            });
        }

    }

    // Fire the on select handler
    if (fire_handler) {
        this.viewer.onDeselectPoint();
    }

}

Racmacs.Point.prototype.highlight = function () {

    this.highlighted++;
    this.updateDisplay();

}
Racmacs.Point.prototype.dehighlight = function () {

    if (this.highlighted > 0) {
        this.highlighted--;
        this.updateDisplay();
    }

}

Racmacs.Point.prototype.updateDisplay = function () {

    // Set styles based on whether points are selected or not
    var styles = this.viewer.styleset.noselections;
    if (this.viewer.points_hovered) styles = this.viewer.styleset.hoveredpoints;
    if (this.viewer.points_selected) styles = this.viewer.styleset.selections;

    // Do not show point if coords are na
    if (this.coords_na) {
        this.hide();
    }

    // Update point appearance
    if (this.hovered) {
        if (this.selected) {
            this.setOutlineColorTemp(styles.hovered.selected.outlineColor);
            this.setOpacity(styles.hovered.selected.opacity);
        } else if (this.highlighted) {
            this.restoreOutlineColor();
            this.setOpacity(styles.hovered.highlighted.opacity);
        } else {
            this.restoreOutlineColor();
            this.setOpacity(styles.hovered.unselected.opacity);
        }
    } else {
        if (this.selected) {
            this.setOutlineColorTemp(styles.unhovered.selected.outlineColor);
            this.setOpacity(styles.unhovered.selected.opacity);
        } else if (this.highlighted) {
            this.restoreOutlineColor();
            this.setOpacity(styles.unhovered.highlighted.opacity);
        } else {
            this.restoreOutlineColor();
            if (this.viewer.points_selected || this.viewer.points_hovered) {
                this.setOpacity(styles.unhovered.unselected.opacity);
            } else {
                this.restoreOpacity();
            }
        }
    }

    // Run any additional on update display events
    this.viewer.dispatchEvent("point-display-updated", {
        point: this
    });

    this.viewer.sceneChange = true;

};










