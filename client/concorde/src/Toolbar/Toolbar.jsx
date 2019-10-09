import React from 'react';
import { connect } from 'react-redux'
import { logout } from '../redux/actions/login';
import '../Concorde.css';

class Toolbar extends React.Component {

    handleLogout = () => {
        this.props.logout({});
    }

    render() {
        return (
            <nav className="navbar navbar-expand-lg navbar-dark bg-dark">
                <span>Concorde</span>
                <div className="collapse navbar-collapse" id="navbarNavAltMarkup">
                </div>
                <button className="btn btn-success" onClick={this.handleLogout}>Logout</button>
            </nav>
            );
    }
}

export default connect(
    null,
    { logout }
)(Toolbar)
