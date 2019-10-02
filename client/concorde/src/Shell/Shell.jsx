import React from 'react';
import '../Concorde.css';

class Shell extends React.Component {

    render() {
        return (
            <div>
                <div className="concorde-shell-output">
                    Concorde 0.1.0
                </div>
                <div class="input-group mb-3">
                    <div class="input-group-prepend">
                        <span class="input-group-text">{localStorage.getItem('admin') ? '&gt;' : '#'}</span>
                    </div>
                    <input type="text" class="form-control" aria-label="Command" />
                </div>
            </div>
        );
    }
}

export { Shell };