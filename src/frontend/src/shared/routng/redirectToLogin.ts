import { redirect } from 'react-router-dom'
import { ROUTE_PATH_LOGIN } from './constants'

export const redirectToLogin = () => redirect(ROUTE_PATH_LOGIN)
