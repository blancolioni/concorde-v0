import React from 'react';
import { Link } from 'react-router-dom';
import '../Concorde.css';

class Toolbar extends React.Component {

    render() {
        return (
            <nav className="navbar navbar-expand-lg navbar-dark bg-dark">
                <span>Concorde</span>
                <div className="collapse navbar-collapse" id="navbarNavAltMarkup">
                </div>
                <Link className="btn btn-success" to="/login">Logout</Link>
            </nav>
            );
    }
}

export { Toolbar };