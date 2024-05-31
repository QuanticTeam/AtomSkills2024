import axios from 'axios'
import { restoreAuthToken } from './auth'

export const http = axios.create({
  baseURL: process.env.REACT_APP_BACKEND_PATH,
  headers: {
    Authorization: `Bearer ${restoreAuthToken()}`,
  },
})
